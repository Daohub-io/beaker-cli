extern crate clap;
extern crate web3;
extern crate rustc_hex;
extern crate ethabi;
extern crate std;

use web3::futures::Future;
use web3::contract::{Contract, Options};
use web3::types::{Address, U256, TransactionReceipt};
use web3::Transport;
use rustc_hex::FromHex;
use ethabi::Token::Uint;

const REQ_CONFIRMATIONS: usize = 1;

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

pub fn register_procedure<T: Transport>(conn:  &EthConn<T>, kernel_contract: &Contract<T>, procedure_address: Address, name: String, caps : Vec<Cap>) -> Result<TransactionReceipt,String> {
    let caps_vals = caps_into_u256s(caps);
    let procedure_key = string_to_proc_key(name);
    let params = (procedure_key, procedure_address, caps_vals);

    // Do a test run of the registration
    let gas_estimate_result: U256 = kernel_contract.estimate_gas("registerAnyProcedure", params.clone(), conn.sender, Options::default()).wait().expect("Gas Estimate");
    println!("Gas Estimate: {:?}", gas_estimate_result);
    let opts = Options::with(|opts| opts.gas = Some(gas_estimate_result));
    // println!("Options: {:?}", &opts);
    let query_result: Result<(ethabi::Token, Address),web3::contract::Error> = kernel_contract.query("registerAnyProcedure", params.clone(), Some(conn.sender), opts.clone(), Some(web3::types::BlockNumber::Latest)).wait();
    match query_result {
        Err(r) => {
            println!("registerAnyProcedure (query) failed");
            println!("{:?}", r);
            return Err(format!("{:?}",r));
        },
        Ok((err_token, _proc_address)) => {
            println!("Register Procedure: {:?}", err_token);
            if let Uint(err_code) = err_token {
                if !err_code.is_zero() {
                    return Err(format!("err_code is not zero, it is {}", err_code));
                }
            } else {
                return Err(format!("err_code is not even a number, it is {:?}", err_token));
            };
        },
    };

    let result = kernel_contract.call_with_confirmations("registerAnyProcedure", params.clone(), conn.sender, opts.clone(), REQ_CONFIRMATIONS).wait();
    match result {
        Err(r) => {
            println!("Transaction failed");
            println!("{:?}", r);
            Err(format!("{:?}",r))
        },
        Ok(receipt) => {
            match receipt.status {
                None => Err(String::from("No status code")),
                Some(web3::types::U64([0])) => {
                    println!("Procedure Registration Receipt: {:?}", receipt);
                    Err(String::from("Registration failed - 0 exit code"))
                    },
                Some(web3::types::U64([1])) => {
                    println!("Procedure Gas Used (Registration): {:?}", receipt.gas_used);
                    println!("Procedure Success: {:?}", receipt.status);
                    Ok(receipt)
                },
                Some(x) => Err(format!("{} is an invalid status code", x))
            }
        }
    }
}

pub fn deploy_example<T: Transport>(conn:  &EthConn<T>) {
    // Deploy a kernel instance
    let kernel_contract = deploy_kernel(conn);

    // Deploying a contract and register it as a procedure
    let caps: Vec<Cap> = vec![Cap::WriteCap{address: U256::from(0x8000), add_keys: U256::from(1)},Cap::LogCap(vec![])];

    deploy_register_procedure(conn, &kernel_contract, String::from("testName"), vec![]).expect("Procedure deployed successfully");
    deploy_register_procedure(conn, &kernel_contract, String::from("another one"), caps.clone()).expect("Procedure deployed successfully");
    deploy_register_procedure(conn, &kernel_contract, String::from("member's procedure"), vec![Cap::WriteCap{address: U256::from(0x8000), add_keys: U256::from(1)},Cap::LogCap(vec![U256::from(0x41)]),Cap::CallCap(Vec::new()),Cap::LogCap(vec![U256::from(0x41),U256::from(0x123456)])]).expect("Procedure deployed successfully");
    deploy_register_procedure(conn, &kernel_contract, String::from("Bob's procedure"), caps.clone()).expect("Procedure deployed successfully");
    deploy_register_procedure(conn, &kernel_contract, String::from("Jane's procedure"), caps.clone()).expect("Procedure deployed successfully");

    kernel_contract.call("setEntryProcedure", (string_to_proc_key(String::from("member's procedure")),), conn.sender, Options::default()).wait().unwrap();
    println!("Kernel Instance Address: {:?}", &kernel_contract.address());
}

pub fn deploy_big_example<T: Transport>(conn:  &EthConn<T>) {
    // Deploy a kernel instance
    let kernel_contract = deploy_kernel(conn);

    // Deploying a contract and register it as a procedure
    let caps: Vec<Cap> = vec![Cap::WriteCap{address: U256::from(0x8000), add_keys: U256::from(1)},Cap::LogCap(vec![])];

    deploy_register_procedure(conn, &kernel_contract, String::from("testName"), vec![]).expect("Procedure deployed successfully");
    deploy_register_procedure(conn, &kernel_contract, String::from("another one"), caps.clone()).expect("Procedure deployed successfully");
    deploy_register_procedure(conn, &kernel_contract, String::from("member's procedure"), vec![Cap::WriteCap{address: U256::from(0x8000), add_keys: U256::from(1)},Cap::LogCap(vec![U256::from(0x41)]),Cap::CallCap(Vec::new()),Cap::LogCap(vec![U256::from(0x41),U256::from(0x123456)])]).expect("Procedure deployed successfully");
    deploy_register_procedure(conn, &kernel_contract, String::from("Bob's procedure"), caps.clone()).expect("Procedure deployed successfully");
    deploy_register_procedure(conn, &kernel_contract, String::from("Jane's procedure"), caps.clone()).expect("Procedure deployed successfully");

    let n_procs = 250;
    for proc_num in 0..n_procs {
        let n_caps = std::cmp::min(32,proc_num);
        // let n_caps = proc_num;

        let these_caps: Vec<Cap> = (0..n_caps).map(|cap_num| Cap::WriteCap{address: U256::from(0x8000+proc_num*n_caps+cap_num), add_keys: U256::from(1)}).collect();
        println!("----------------------------------------------");
        println!("Registering Procedure #{} with {} capabilities", proc_num, n_caps);
        deploy_register_procedure(conn, &kernel_contract, String::from(format!("Jane's proc #{}",proc_num)), these_caps).expect("Procedure deployed successfully");
        println!("----------------------------------------------");
    }
    kernel_contract.call("setEntryProcedure", (string_to_proc_key(String::from("member's procedure")),), conn.sender, Options::default()).wait().unwrap();
    println!("Kernel Instance Address: {:?}", &kernel_contract.address());
}

pub fn deploy_kernel<T: Transport>(conn:  &EthConn<T>) -> Contract<T> {
    // Deploying a kernel instance
    let kernel_code: Vec<u8> = include_str!("../../Kernel/Kernel.bin").from_hex().unwrap();
    let (kernel_contract, kernel_receipt) = Contract::deploy(conn.web3.eth(), include_bytes!("../../Kernel/Kernel.abi"))
            .unwrap()
            .confirmations(REQ_CONFIRMATIONS)
            .options(Options::with(|opt| {
                opt.gas = Some(4_700_000.into())
            }))
            .execute(
                kernel_code,
                ( ),
                conn.sender,
            )
            .expect("Correct parameters are passed to the constructor.")
            .wait()
            .unwrap();
    println!("Kernel Instance Address: {:?}", kernel_contract.address());
    let web3::types::Bytes(code_vec_kernel)= conn.web3.eth().code(kernel_contract.address(), None).wait().unwrap();
    println!("Kernel Code Length: {:?}", code_vec_kernel.len());
    println!("Kernel Gas Used (Deployment): {:?}", kernel_receipt.gas_used);

    // A quick test to make sure we can execute functions in the kernel
    let test_result : U256 = kernel_contract.query("testGetter", ( ), None, Options::default(), None).wait().unwrap();
    assert_eq!(test_result, 3.into());
    kernel_contract
}

pub fn deploy_proc<T: Transport>(conn:  &EthConn<T>, kernel_address: Address, proc_code_path: String, proc_abi_path: String, name: String) {
    println!("about to deploy proc");
    let kernel_abi = include_bytes!("../../Kernel/Kernel.abi");
    let kernel_contract = match Contract::from_json(conn.web3.eth(), kernel_address, kernel_abi) {
            Err(r) => {
                panic!("unable to build kernel contract: {:?}", r);
            },
            Ok(con) => con,
        };
    deploy_register_procedure(conn, &kernel_contract, name, vec![]).expect("Procedure deployed successfully");
}

pub fn deploy_register_procedure<T: Transport>(conn:  &EthConn<T>, kernel_contract: &Contract<T>, name: String, caps : Vec<Cap>) -> Result<TransactionReceipt,String> {
    // Deploy the procedure
    let example_code: Vec<u8> = include_str!("../../Adder/Adder.bin").from_hex().unwrap();
        // Deploying a contract
    let (example_contract, example_receipt) = Contract::deploy(conn.web3.eth(), include_bytes!("../../Adder/Adder.abi"))
            .unwrap()
            .confirmations(REQ_CONFIRMATIONS)
            .options(Options::with(|opt| {
                opt.gas = Some(3_000_000.into())
            }))
            .execute(
                example_code,
                ( ),
                conn.sender,
            )
            .expect("Correct parameters are passed to the constructor.")
            // If we pass this wait to the parent we can do faster batch jobs
            .wait()
            .unwrap();
    println!("Procedure Address: {:?}", example_contract.address());
    let web3::types::Bytes(code_vec_example)= conn.web3.eth().code(example_contract.address(), None).wait().expect("Procedure code should be retrieved");
    println!("Procedure Code Length: {:?}", code_vec_example.len());
    println!("Procedure Gas Used (Deployment): {:?}", example_receipt.gas_used);
    register_procedure(conn, kernel_contract, example_contract.address(), name, caps)
}

pub struct EthConn<T: Transport> {
    pub web3: web3::api::Web3<T>,
    pub sender: Address,
}

#[derive(Clone)]
pub enum Cap {
    WriteCap {address: U256, add_keys: U256},
    RegisterCap,
    CallCap(Vec<U256>),
    LogCap(Vec<U256>), // vec is of length 0-4
}

impl Cap {
    fn to_u256s(&self) -> Vec<U256> {
        match self {
            Cap::WriteCap {address, add_keys} => vec![/* length */ U256::from(3), /* type */ U256::from(7),U256::from(address),U256::from(add_keys)],
            Cap::RegisterCap => vec![/* length */ U256::from(1), /* type */ U256::from(11)],
            Cap::LogCap(topics) => {
                let mut v = vec![/* length */ U256::from(1+topics.len()), /* type */ U256::from(9)];
                v.extend(topics);
                v
                },
            Cap::CallCap(keys) => vec![/* length */ U256::from(1), /* type */ U256::from(3)],
        }
    }
}

fn caps_into_u256s(caps: Vec<Cap>) -> Vec<U256> {
    concat_vecs(caps.iter().map(|c| {c.to_u256s()}).collect())
}

fn concat_vecs(vecs: Vec<Vec<U256>>) -> Vec<U256> {
    let size = vecs.iter().fold(0, |a, b| a + b.len());
    vecs.into_iter().fold(Vec::with_capacity(size), |mut acc, v| {
        acc.extend(v); acc
    })
}


#[cfg(test)]
mod deploy_tests {

    use super::*;
    use web3::futures::Future;
    use web3::contract::{Contract, Options};
    use web3::types::{Address, U256};
    use web3::Transport;
    use rustc_hex::FromHex;
    use ethabi::Token::Uint;

    #[test]
    fn deploying_kernel() {
        let (_eloop, transport) = web3::transports::Http::new("http://localhost:8545").expect("Connection built");
        let web3 = web3::Web3::new(transport);
        let sender = match web3.eth().accounts().wait() {
            Err(_r) => {
                panic!("No Ethereum network available");
                },
            Ok(x) => x[0],
        };
        let conn = &EthConn {
            web3,
            sender
        };
        // Deploy a kernel instance
        let kernel_contract = deploy_kernel(conn);
    }

    /// Each of these write caps is 4 keys long. As the maximum length of
    /// the cap table is 128, the most we can have is 32 (32*4=128). Therefore,
    /// this example with 32 write caps should succeed.
    #[test]
    fn deploying_proc_32_caps() {
        let (_eloop, transport) = web3::transports::Http::new("http://localhost:8545").expect("Connection built");
        let web3 = web3::Web3::new(transport);
        let sender = match web3.eth().accounts().wait() {
            Err(_r) => {
                panic!("No Ethereum network available");
                },
            Ok(x) => x[0],
        };
        let conn = &EthConn {
            web3,
            sender
        };
        // Deploy a kernel instance
        let kernel_contract = deploy_kernel(conn);

        let x_caps = 32;
        let those_caps: Vec<Cap> = (0..x_caps).map(|cap_num| Cap::WriteCap{address: U256::from(0x1000+cap_num), add_keys: U256::from(1)}).collect();
        deploy_register_procedure(conn, &kernel_contract, String::from(format!("Jane's proc #{}",x_caps)), those_caps).expect("Procedure deployed successfully");
    }

    /// Each of these write caps is 4 keys long. As the maximum length of
    /// the cap table is 128, the most we can have is 32 (32*4=128). Therefore,
    /// this example with 33 write caps should fail.
    #[test]
    fn deploying_proc_33_caps() {
        let (_eloop, transport) = web3::transports::Http::new("http://localhost:8545").expect("Connection built");
        let web3 = web3::Web3::new(transport);
        let sender = match web3.eth().accounts().wait() {
            Err(_r) => {
                panic!("No Ethereum network available");
                },
            Ok(x) => x[0],
        };
        let conn = &EthConn {
            web3,
            sender
        };
        // Deploy a kernel instance
        let kernel_contract = deploy_kernel(conn);

        let x_caps = 33;
        let those_caps: Vec<Cap> = (0..x_caps).map(|cap_num| Cap::WriteCap{address: U256::from(0x1000+cap_num), add_keys: U256::from(1)}).collect();
        deploy_register_procedure(conn, &kernel_contract, String::from(format!("Jane's proc #{}",x_caps)), those_caps).expect_err("Procedure not deployed successfully");
    }


    #[test]
    fn deploying_512_procs() {
        let (_eloop, transport) = web3::transports::Http::new("http://localhost:8545").unwrap();
        let web3 = web3::Web3::new(transport);
        let sender = match web3.eth().accounts().wait() {
            Err(_r) => {
                panic!("No Ethereum network available");
                },
            Ok(x) => x[0],
        };
        let conn = &EthConn {
            web3,
            sender
        };
        // Deploy a kernel instance
        let kernel_contract = deploy_kernel(conn);

        let n_procs = 512;
        for proc_num in 0..n_procs {
            let n_caps = 1;
            let these_caps: Vec<Cap> = (0..n_caps).map(|cap_num| Cap::WriteCap{address: U256::from(0x8000+proc_num*n_caps+cap_num), add_keys: U256::from(1)}).collect();
            deploy_register_procedure(conn, &kernel_contract, String::from(format!("Jane's proc #{}",proc_num)), these_caps).expect("Procedure deployed successfully");
        }
        kernel_contract.call("setEntryProcedure", (string_to_proc_key(String::from("member's procedure")),), conn.sender, Options::default()).wait().unwrap();
    }

    #[test]
    fn deploying_512_procs_512_caps() {
        let (_eloop, transport) = web3::transports::Http::new("http://localhost:8545").unwrap();
        let web3 = web3::Web3::new(transport);
        let sender = match web3.eth().accounts().wait() {
            Err(_r) => {
                panic!("No Ethereum network available");
                },
            Ok(x) => x[0],
        };
        let conn = &EthConn {
            web3,
            sender
        };
        // Deploy a kernel instance
        let kernel_contract = deploy_kernel(conn);
        let n_procs = 512;
        for proc_num in 0..n_procs {
            let n_caps = proc_num;
            let these_caps: Vec<Cap> = (0..n_caps).map(|cap_num| Cap::WriteCap{address: U256::from(0x8000+proc_num*n_caps+cap_num), add_keys: U256::from(1)}).collect();
            deploy_register_procedure(conn, &kernel_contract, String::from(format!("Jane's proc #{}",proc_num)), these_caps).expect("Procedure deployed successfully");
        }
        kernel_contract.call("setEntryProcedure", (string_to_proc_key(String::from("member's procedure")),), conn.sender, Options::default()).wait().unwrap();
    }
}