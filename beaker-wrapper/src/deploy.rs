extern crate clap;
extern crate web3;
extern crate rustc_hex;
extern crate ethabi;
extern crate std;

use clap::{Arg, App, SubCommand};
use std::process::Command;
use std::str::FromStr;
use web3::futures::Future;
use web3::contract::{Contract, Options};
use web3::types::{Address, U256};
use rustc_hex::FromHex;
use ethabi::Token;
use ethabi::Token::Uint;

pub fn string_to_proc_key(mut name: String) -> [u8; 24] {
    if !name.is_ascii() {
        println!("{}", name);
        panic!("name is not ascii");
    }
    if name.len() > 24 {
        println!("{}", name);
        panic!("name ({}) is greater than 24 characters, it is {} characters", name, name.len());
    }
    name.push_str("\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
    name.truncate(24);
    let mut procedure_key : [u8; 24] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
    let byte_name = name.into_bytes();
    procedure_key.clone_from_slice(&byte_name[..24]);
    procedure_key
}

pub fn register_procedure(web3: &web3::api::Web3<web3::transports::Http>, kernel_contract: &Contract<web3::transports::Http>, sender: Address, procedure_address: Address, name: String, caps : Vec<U256>) {
    let procedure_key = string_to_proc_key(name);
    let params = (procedure_key, procedure_address, caps);

    // Do a test run of the registration
    let query_result : (ethabi::Token, Address) = match kernel_contract.query("registerAnyProcedure", params.clone(), Some(sender), Options::default(), Some(web3::types::BlockNumber::Latest)).wait() {
            Err(r) => {
                println!("registerAnyProcedure (query) failed");
                println!("{:?}", r);
                std::process::exit(1);
            },
            Ok(x) => x,
    };
    println!("Register Procedure: {:?}", query_result);
    let (err_token, proc_address) = query_result;
    if let Uint(err_code) = err_token {
        if !err_code.is_zero() {
            panic!("err_code is not zero, it is {}", err_code);
        }
    } else {
        panic!("err_code is not even a number");
    }

    let result = kernel_contract.call("registerAnyProcedure", params.clone(), sender, Options::default()).wait();
    match result {
        Err(r) => {
            println!("Transaction failed");
            println!("{:?}", r);
            std::process::exit(1);
        },
        Ok(tx) => {
            let tx_r = web3.eth().transaction_receipt(tx).wait();
            match tx_r {
                Err(_r) => println!("Could not retrieve transaction receipt"),
                Ok(rec) => match rec {
                    None => println!("No receipt"),
                    Some(receipt) => println!("Procedure Gas Used (Registration): {:?}", receipt.gas_used),
                }
            }
        }
    }
}

pub fn deploy_example(web3: &web3::api::Web3<web3::transports::Http>) {
    let sender = match web3.eth().accounts().wait() {
        Err(_r) => {
            println!("No Ethereum network available");
            std::process::exit(1);
            },
        Ok(x) => x[0],
    };

    // Deploy a kernel instance
    let kernel_contract = deploy_kernel(&web3, sender);

    // Deploying a contract and register it as a procedure
    let caps : Vec<U256> = vec![U256::from(3),U256::from(7),U256::from(0x8000),U256::from(1)];
    deploy_register_procedure(&web3, &kernel_contract, sender, String::from("testName"), caps.clone());
    deploy_register_procedure(&web3, &kernel_contract, sender, String::from("another one"), caps.clone());
    deploy_register_procedure(&web3, &kernel_contract, sender, String::from("member's procedure"), vec![U256::from(3),U256::from(7),U256::from(0x8000),U256::from(1),U256::from(2),U256::from(9),U256::from(0),U256::from(1),U256::from(3)]);
    deploy_register_procedure(&web3, &kernel_contract, sender, String::from("Bob's procedure"), caps.clone());
    deploy_register_procedure(&web3, &kernel_contract, sender, String::from("Jane's procedure"), caps.clone());

    kernel_contract.call("setEntryProcedure", (string_to_proc_key(String::from("member's procedure"))), sender, Options::default()).wait().unwrap();
}

pub fn deploy_kernel(web3: &web3::api::Web3<web3::transports::Http>, sender: Address) -> Contract<web3::transports::Http> {
    // Deploying a kernel instance
    let kernel_code: Vec<u8> = include_str!("../../Kernel/Kernel.bin").from_hex().unwrap();
    let (kernel_contract, kernel_receipt) = Contract::deploy(web3.eth(), include_bytes!("../../Kernel/Kernel.abi"))
            .unwrap()
            .confirmations(1)
            .options(Options::with(|opt| {
                opt.gas = Some(4_700_000.into())
            }))
            .execute(
                kernel_code,
                ( ),
                sender,
            )
            .expect("Correct parameters are passed to the constructor.")
            .wait()
            .unwrap();
    println!("Kernel Instance Address: {:?}", kernel_contract.address());
    let web3::types::Bytes(code_vec_kernel)= web3.eth().code(kernel_contract.address(), None).wait().unwrap();
    println!("Kernel Code Length: {:?}", code_vec_kernel.len());
    println!("Kernel Gas Used (Deployment): {:?}", kernel_receipt.gas_used);

    // A quick test to make sure we can execute functions in the kernel
    let test_result : U256 = kernel_contract.query("testGetter", ( ), None, Options::default(), None).wait().unwrap();
    assert_eq!(test_result, 3.into());
    kernel_contract
}

pub fn deploy_proc(web3: &web3::api::Web3<web3::transports::Http>, kernel_address: Address, proc_code_path: String, proc_abi_path: String, name: String) {
    // First check that there is a local net
    let (_eloop, transport) = web3::transports::Http::new("http://localhost:8545").unwrap();
    let web3 = web3::Web3::new(transport);
    let sender = match web3.eth().accounts().wait() {
        Err(_r) => {
            println!("No Ethereum network available");
            std::process::exit(1);
            },
        Ok(x) => x[0],
    };
    println!("about to deploy proc");
    let kernel_abi = include_bytes!("../../Kernel/Kernel.abi");
    let kernel_contract = match Contract::from_json(web3.eth(), kernel_address, kernel_abi) {
            Err(r) => {
                panic!("unable to build kernel contract: {:?}", r);
            },
            Ok(con) => con,
        };
    deploy_register_procedure(&web3, &kernel_contract, sender, name, vec![])
}

pub fn deploy_register_procedure(web3: &web3::api::Web3<web3::transports::Http>, kernel_contract: &Contract<web3::transports::Http>, sender: Address, name: String, caps : Vec<U256>) {
    // Deploy the procedure
    let example_code: Vec<u8> = include_str!("../../Adder/Adder.bin").from_hex().unwrap();
        // Deploying a contract
    let (example_contract, example_receipt) = Contract::deploy(web3.eth(), include_bytes!("../../Adder/Adder.abi"))
            .unwrap()
            .confirmations(1)
            .options(Options::with(|opt| {
                opt.gas = Some(3_000_000.into())
            }))
            .execute(
                example_code,
                ( ),
                sender,
            )
            .expect("Correct parameters are passed to the constructor.")
            .wait()
            .unwrap();
    println!("Procedure Address: {:?}", example_contract.address());
    let web3::types::Bytes(code_vec_example)= web3.eth().code(example_contract.address(), None).wait().unwrap();
    println!("Procedure Code Length: {:?}", code_vec_example.len());
    println!("Procedure Gas Used (Deployment): {:?}", example_receipt.gas_used);
    register_procedure(web3, kernel_contract, sender, example_contract.address(), name, caps);
}
