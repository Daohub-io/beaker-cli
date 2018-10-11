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

pub fn register_procedure(web3: web3::api::Web3<web3::transports::Http>, kernel_contract: &Contract<web3::transports::Http>, sender: Address, procedure_address: Address, name_spec: String, caps : Vec<U256>) {
    let mut name = name_spec.clone();
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
    let byte_name = name.into_bytes();
    let mut procedure_key : [u8; 24] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
    procedure_key[0] = byte_name[0];
    procedure_key[1] = byte_name[1];
    procedure_key[2] = byte_name[2];
    procedure_key[3] = byte_name[3];
    procedure_key[4] = byte_name[4];
    procedure_key[5] = byte_name[5];
    procedure_key[6] = byte_name[6];
    procedure_key[7] = byte_name[7];
    procedure_key[8] = byte_name[8];
    procedure_key[9] = byte_name[9];
    procedure_key[10] = byte_name[10];
    procedure_key[11] = byte_name[11];
    procedure_key[12] = byte_name[12];
    procedure_key[13] = byte_name[13];
    procedure_key[14] = byte_name[14];
    procedure_key[15] = byte_name[15];
    procedure_key[16] = byte_name[16];
    procedure_key[17] = byte_name[17];
    procedure_key[18] = byte_name[18];
    procedure_key[19] = byte_name[19];
    procedure_key[20] = byte_name[20];
    procedure_key[21] = byte_name[21];
    procedure_key[22] = byte_name[22];
    procedure_key[23] = byte_name[23];
    let params = (procedure_key, procedure_address, caps);

    let result2 : (ethabi::Token, Address) = kernel_contract.query("registerAnyProcedure", params.clone(), Some(sender), Options::default(), Some(web3::types::BlockNumber::Latest)).wait().unwrap();
    println!("Register Procedure: {:?}", result2);

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

pub fn deploy_example() {
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

    // Deploy a kernel instance
    let kernel_contract = deploy_kernel();

    // Deploying a contract and register it as a procedure
    let caps : Vec<U256> = vec![U256::from(3),U256::from(7),U256::from(0x8000),U256::from(1)];
    deploy_register_procedure(web3.clone(), &kernel_contract, sender, String::from("testName"), caps.clone());
    deploy_register_procedure(web3.clone(), &kernel_contract, sender, String::from("another one"), caps.clone());
    deploy_register_procedure(web3.clone(), &kernel_contract, sender, String::from("member's procedure"), vec![U256::from(3),U256::from(7),U256::from(0x8000),U256::from(1),U256::from(2),U256::from(3),U256::from(0)]);
    deploy_register_procedure(web3.clone(), &kernel_contract, sender, String::from("Bob's procedure"), caps.clone());
    deploy_register_procedure(web3.clone(), &kernel_contract, sender, String::from("Jane's procedure"), caps.clone());
}

pub fn deploy_kernel() -> Contract<web3::transports::Http> {
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
    // Deploying a kernel instance
    let kernel_code: Vec<u8> = include_str!("../../Kernel/Kernel.bin").from_hex().unwrap();
    let (kernel_contract, kernel_receipt) = Contract::deploy(web3.eth(), include_bytes!("../../Kernel/Kernel.abi"))
            .unwrap()
            .confirmations(1)
            .options(Options::with(|opt| {
                opt.gas = Some(4_823_000.into())
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

pub fn deploy_proc(kernel_address: Address, proc_code_path: String, proc_abi_path: String, name: String) {
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
    deploy_register_procedure(web3, &kernel_contract, sender, name, vec![])
}

pub fn deploy_register_procedure(web3: web3::api::Web3<web3::transports::Http>, kernel_contract: &Contract<web3::transports::Http>, sender: Address, name: String, caps : Vec<U256>) {
    // Deploy the procedure
    let example_code: Vec<u8> = include_str!("../../Adder/Adder.bin").from_hex().unwrap();
    let web32 = web3.clone();
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
    let web3::types::Bytes(code_vec_example)= web32.eth().code(example_contract.address(), None).wait().unwrap();
    println!("Procedure Code Length: {:?}", code_vec_example.len());
    println!("Procedure Gas Used (Deployment): {:?}", example_receipt.gas_used);
    register_procedure(web3, kernel_contract, sender, example_contract.address(), name, caps);
}
