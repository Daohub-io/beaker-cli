extern crate clap;
use clap::{Arg, App, SubCommand};
use std::process::Command;
use std::str::FromStr;

fn main() {
    let matches = App::new("Beaker CLI")
                        .version("0.2.0")
                        .author("Daolab <info@daolab.io>")
                        .about("A command-line interface for BeakerOS on the Ethereum blockchain.")
                        //   .arg(Arg::with_name("config")
                        //        .short("c")
                        //        .long("config")
                        //        .value_name("FILE")
                        //        .help("Sets a custom config file")
                        //        .takes_value(true))
                        //   .arg(Arg::with_name("INPUT")
                        //        .help("Sets the input file to use")
                        //        .required(true)
                        //        .index(1))
                        //   .arg(Arg::with_name("v")
                        //        .short("v")
                        //        .multiple(true)
                        //        .help("Sets the level of verbosity"))
                        .subcommand(SubCommand::with_name("check")
                            .about("Check that the procedure only contains beaker-compliant opcodes")
                            .arg(Arg::with_name("INPUT-FILE")
                                .required(true)
                                .help("input file")))
                        .subcommand(SubCommand::with_name("compile")
                            .about("The original compile function")
                            .arg(Arg::with_name("INPUT-FILE")
                                .required(true)
                                .help("input file"))
                            .arg(Arg::with_name("OUTPUT")
                                .required(false)
                                .help("input file"))
                            .arg(Arg::with_name("write")
                               .long("write")
                               .value_name("ARG")
                               .help("Sets the write capabilities")
                               .takes_value(true)))
                        .subcommand(SubCommand::with_name("opcodes")
                            .about("Print opcodes")
                            .arg(Arg::with_name("INPUT-FILE")
                                .required(true)
                                .help("input file"))
                            .arg(Arg::with_name("read")
                                .short("r")
                                .long("read")
                                .value_name("READ-TYPE")
                                .required(false)
                                .help("Type of read input")))
                        .subcommand(SubCommand::with_name("structures")
                            .about("Print structures")
                            .arg(Arg::with_name("INPUT-FILE")
                                .required(true)
                                .help("input file"))
                            .arg(Arg::with_name("read")
                                .short("r")
                                .long("read")
                                .value_name("READ-TYPE")
                                .required(false)
                                .help("Type of read input")))
                        .subcommand(SubCommand::with_name("deploy")
                            .about("Deploy a kernel to the chain"))
                        .subcommand(SubCommand::with_name("status")
                            .about("Get the status of a deployed kernel")
                            .arg(Arg::with_name("KERNEL-ADDRESS")
                                .required(true)
                                .help("the Ethereum address of the kernel")))
                        .get_matches();

    // // Gets a value for config if supplied by user, or defaults to "default.conf"
    // let config = matches.value_of("config").unwrap_or("default.conf");
    // println!("Value for config: {}", config);

    // // Calling .unwrap() is safe here because "INPUT" is required (if "INPUT" wasn't
    // // required we could have used an 'if let' to conditionally get the value)
    // println!("Using input file: {}", matches.value_of("INPUT").unwrap());

    // Subcommands
    let output = if let Some(_matches) = matches.subcommand_matches("deploy") {
        Some(run_command_direct(&["deploy"]))
    } else if let Some(matches) = matches.subcommand_matches("status") {
        let address = matches.value_of("KERNEL-ADDRESS").unwrap();
        Some(run_command_direct(&["status", address]))
    } else if let Some(matches) = matches.subcommand_matches("compile") {
        let address = matches.value_of("INPUT-FILE").unwrap();
        Some(run_command_direct(&["compile", address]))
    } else if let Some(matches) = matches.subcommand_matches("opcodes") {
        let address = matches.value_of("INPUT-FILE").unwrap();
        let read : &str = match matches.value_of("read") {
            Some(x) => x,
            None => "hex",
        };
        Some(run_command_direct(&["opcodes", address, "--read", read]))
    } else if let Some(matches) = matches.subcommand_matches("structures") {
        let address = matches.value_of("INPUT-FILE").unwrap();
        let read : &str = match matches.value_of("read") {
            Some(x) => x,
            None => "hex",
        };
        Some(run_command_direct(&["structures", address, "--read", read]))
    } else {
        None
    };

    match output {
        Some(output) => {
            if !output.success() {
                println!("execution failed");
            }
        },
        None => println!("No valid command given"),
    }
}

fn run_command_direct(sub_args: &[&str]) -> std::process::ExitStatus {
    let mut full_args = vec!["exec", "--", "beaker"];
    full_args.extend(sub_args);
    return if cfg!(target_os = "windows") {
        Command::new("stack")
            .args(full_args)
            .status()
            .expect("failed to execute process")
    } else {
        Command::new("stack")
                .args(full_args)
                .status()
                .expect("failed to execute process")
    };
}

enum ReadType {
    Binary,
    Hex,
    SolC,
}

impl FromStr for ReadType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bin" => Ok(ReadType::Binary),
            "hex" => Ok(ReadType::Hex),
            "solc" => Ok(ReadType::SolC),
            _ => Err(String::from("Could not parse read type")),
        }
    }
}