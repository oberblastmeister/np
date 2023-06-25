use anyhow::{anyhow, Result};
use clap::{ArgAction, Parser};
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{de, Deserialize, Deserializer};
use skim::prelude::*;
use std::ffi::OsStr;
use std::io::{ BufRead, BufReader};
use std::process::{self, Command, Stdio};
use std::{fmt, panic, thread};
use xshell::{cmd, Shell};

#[derive(Deserialize)]
pub struct ItemData {
    pname: String,
    description: String,
    version: String,
}
pub struct Item {
    attrpath: String,
    item: ItemData,
    flake: String,
}

impl SkimItem for Item {
    fn text(&self) -> Cow<str> {
        format!("{}", self.attrpath).into()
    }

    fn display<'a>(&'a self, context: DisplayContext<'a>) -> AnsiString<'a> {
        let item = &self.item;
        format!("{} ({}): {}", self.attrpath, item.version, item.description).into()
    }

    fn output(&self) -> Cow<str> {
        format!("{}#{}", self.flake, self.attrpath.as_str()).into()
    }
}

#[derive(Parser)]
pub struct Install {
    #[arg(short = 'f', long = "flake", action = ArgAction::Append)]
    flakes: Vec<String>,

    packages: Vec<String>,
}

impl Install {
    pub fn run(&self) -> Result<()> {
        let sh = Shell::new()?;
        let flakes = if self.flakes.is_empty() && self.packages.is_empty() {
            Cow::Owned(Vec::from([String::from("nixpkgs")]))
        } else {
            Cow::Borrowed(&self.flakes)
        };

        let mut packages = fuzzy_install(&flakes)?
            .into_iter()
            .map(|t| t.output().into_owned())
            .collect::<Vec<_>>();
        packages.extend(self.packages.iter().cloned());
        if packages.is_empty() {
            return Ok(());
        }
        cmd!(sh, "nix profile install {packages...}").run()?;
        Ok(())
    }
}

#[derive(Parser)]
pub struct Remove {}

struct NixProfileLine {
    index: u32,
    data1: String,
    data2: String,
    store_path: String,
}

impl SkimItem for NixProfileLine {
    fn text(&self) -> Cow<str> {
        Cow::Borrowed(&self.data1)
    }

    fn output(&self) -> Cow<str> {
        Cow::Borrowed(&self.store_path)
    }
}

impl NixProfileLine {
    fn parse(s: &str) -> Option<NixProfileLine> {
        static RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"(\d+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)").unwrap());
        let captures = RE.captures(s)?;
        let index = captures.get(1)?.as_str().parse::<u32>().ok()?;
        let data1 = captures.get(2)?.as_str().to_owned();
        let data2 = captures.get(3)?.as_str().to_owned();
        let store_path = captures.get(4)?.as_str().to_owned();
        Some(NixProfileLine {
            index,
            data1,
            data2,
            store_path,
        })
    }
}

fn get_nix_profile_installed_store_paths(sh: &Shell) -> Result<Vec<String>> {
    let res = run_skim_items(
        get_nix_profile_installed(&sh)?
            .into_iter()
            .map(|t| t as Arc<dyn SkimItem>)
            .collect::<Vec<_>>(),
    )?;
    let paths = res
        .iter()
        .map(|t| t.output().to_string())
        .collect::<Vec<_>>();
    Ok(paths)
}

fn get_nix_profile_installed(sh: &Shell) -> Result<Vec<Arc<NixProfileLine>>> {
    let output = cmd!(sh, "nix profile list").output()?;
    output
        .stdout
        .lines()
        .map(|l| {
            let data = NixProfileLine::parse(&l?).ok_or(anyhow!("Failed to parse data"))?;
            Ok(Arc::new(data))
        })
        .collect()
}

impl Remove {
    pub fn run(&self) -> Result<()> {
        let sh = Shell::new()?;
        let paths = get_nix_profile_installed_store_paths(&sh)?;
        if paths.is_empty() {
            return Ok(());
        }
        cmd!(sh, "nix profile remove {paths...}").run()?;
        Ok(())
    }
}

#[derive(Parser)]
pub struct Upgrade {}

impl Upgrade {
    pub fn run(&self) -> Result<()> {
        let sh = Shell::new()?;
        let paths = get_nix_profile_installed_store_paths(&sh)?;
        if paths.is_empty() {
            return Ok(());
        }
        cmd!(sh, "nix profile upgrade {paths...}").run()?;
        Ok(())
    }
}

#[derive(Parser)]
pub enum Cmd {
    List,
    Install(Install),
    Remove(Remove),
    Upgrade(Upgrade),
}

pub fn main() -> Result<()> {
    let cmd = Cmd::parse();
    match cmd {
        Cmd::Install(t) => t.run(),
        Cmd::Remove(t) => t.run(),
        Cmd::Upgrade(t) => t.run(),
        Cmd::List => {
            let sh = Shell::new()?;
            for data in get_nix_profile_installed(&sh)? {
                if data.data1 == "-" {
                    continue;
                }
                println!("{}", data.data1);
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn run_command_stdout<I, S>(command: &str, args: I) -> Result<process::ChildStdout>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    Command::new(command)
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?
        .stdout
        .ok_or(anyhow!("No stdout"))
}

fn send_packages<'s, 'env>(
    s: &'s thread::Scope<'s, 'env>,
    flake: &'s str,
    sender: SkimItemSender,
) -> thread::ScopedJoinHandle<'s, Result<()>> {
    s.spawn(move || {
        let stdout = run_command_stdout("nix", ["search", flake, "--json"])?;
        let buf_reader = BufReader::new(stdout);
        struct VecMapVisitor(SkimItemSender, String);

        impl<'de> de::Visitor<'de> for VecMapVisitor {
            type Value = ();

            fn expecting(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt.write_str("a map")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                while let Some((attrpath, item)) = map.next_entry()? {
                    let item: Arc<dyn SkimItem> = Arc::new(Item {
                        attrpath,
                        item,
                        flake: self.1.clone(),
                    });
                    match self.0.send(item) {
                        Err(_) => break,
                        _ => (),
                    };
                }
                Ok(())
            }
        }
        let mut de = serde_json::Deserializer::from_reader(buf_reader);
        de.deserialize_map(VecMapVisitor(sender, String::from(flake)))?;

        Ok::<_, anyhow::Error>(())
    })
}

fn fuzzy_install(flakes: &[String]) -> Result<Vec<Arc<dyn SkimItem>>> {
    if flakes.is_empty() {
        return Ok(Vec::new());
    }

    thread::scope(|s| {
        let (sender, receiver) = unbounded();

        let mut joins = Vec::with_capacity(flakes.len());

        for flake in flakes {
            joins.push(send_packages(s, flake, sender.clone()))
        }
        // need to make sure to drop the sender so that there is no 100% cpu usage
        drop(sender);

        let res = run_skim(receiver)?;

        for join in joins {
            join.join().unwrap_or_else(|e| panic::resume_unwind(e))?;
        }

        Ok(res)
    })
}

fn run_skim_items(items: Vec<Arc<dyn SkimItem>>) -> Result<Vec<Arc<dyn SkimItem>>> {
    let (sender, receiver) = bounded(items.len());
    for item in items {
        sender.send(item).unwrap();
    }
    drop(sender);
    run_skim(receiver)
}

fn run_skim(receiver: SkimItemReceiver) -> Result<Vec<Arc<dyn SkimItem>>> {
    let options = SkimOptionsBuilder::default()
        // .height(Some("50%"))
        .multi(true)
        .build()
        .unwrap();
    // `run_with` would read and show items from the stream
    let skim_output = Skim::run_with(&options, Some(receiver)).ok_or(anyhow!("Skim failed"))?;
    println!("\x1b[0m");
    if skim_output.is_abort {
        return Ok(Vec::new());
    }
    let res = skim_output.selected_items;
    Ok(res)
}
