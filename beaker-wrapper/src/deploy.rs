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

pub fn register_procedure(web3: web3::api::Web3<web3::transports::Http>, kernel_contract: Contract<web3::transports::Http>, sender: Address, procedure_address: Address) {
    let caps : Vec<U256> = vec![U256::from(3),U256::from(7),U256::from(0x8000),U256::from(1)];
    let procedure_key : [u8; 24] = [0x75, 0x65, 0x73, 0x74, 0x4e, 0x61, 0x6d, 0x65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];//String::from("testName0000000000000000").into_bytes();
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
            // let tx_r = web3.eth().transaction_receipt(tx).wait();
            // println!("{:?}", tx_r);
            println!("Transaction: {:?}", tx);
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

        // Deploying a kernel instance
    let kernel_code: Vec<u8> = include_str!("../../Kernel/Kernel.bin").from_hex().unwrap();
    let kernel_contract = Contract::deploy(web3.eth(), include_bytes!("../../Kernel/Kernel.abi"))
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

    // A quick test to make sure we can execute functions in the kernel
    let testResult : U256 = kernel_contract.query("testGetter", ( ), None, Options::default(), None).wait().unwrap();
    assert_eq!(testResult, 3.into());

    // Deploy an example procedure
    let example_code: Vec<u8> = include_str!("../../Adder/Adder.bin").from_hex().unwrap();
    let web32 = web3.clone();
        // Deploying a contract
    let example_contract = Contract::deploy(web3.eth(), include_bytes!("../../Adder/Adder.abi"))
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
    register_procedure(web3, kernel_contract, sender, example_contract.address());
    println!("Example Procedure Address: {:?}", example_contract.address());
    let web3::types::Bytes(code_vec_example)= web32.eth().code(example_contract.address(), None).wait().unwrap();
    println!("Example Procedure Code Length: {:?}", code_vec_example.len());
}
