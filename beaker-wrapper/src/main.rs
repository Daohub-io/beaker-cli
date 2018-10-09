extern crate clap;
use clap::{Arg, App, SubCommand};
use std::process::Command;

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
                          .subcommand(SubCommand::with_name("deploy")
                                      .about("deploys a kernel to the chain")
                                    //   .version("1.3")
                                    //   .author("Someone E. <someone_else@other.com>")

                          )
                          .subcommand(SubCommand::with_name("status")
                                      .about("get the status of a deployed kernel")
                                      .arg(Arg::with_name("KERNEL-ADDRESS")
                                          .required(true)
                                          .help("the Ethereum address of the kernel"))
                                    //   .version("1.3")
                                    //   .author("Someone E. <someone_else@other.com>")
                                    //   .arg(Arg::with_name("debug")
                                    //       .short("d")
                                    //       .help("print debug information verbosely")))
                          )
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