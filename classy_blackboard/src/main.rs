pub fn main() {
    std::process::Command::new("cargo")
        .args(["test"])
        .status()
        .unwrap();
}
