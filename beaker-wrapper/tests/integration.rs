extern crate assert_cli;

#[cfg(test)]
mod integration {
    use assert_cli;

    #[test]
    fn calling_beaker_without_args() {
        assert_cli::Assert::main_binary()
            .stdout().is("No valid command given")
            .unwrap();
    }

    #[test]
    fn calling_deploy_example() {
        assert_cli::Assert::command(&["cargo", "run", "--", "deploy"])
            .unwrap();
    }
}
