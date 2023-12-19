fn main() {
    let parser_generator_dir =
        std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap())
            .join("tree-sitter-ridotto");

    let grammar_js = parser_generator_dir.join("grammar.js");
    println!("cargo:rerun-if-changed={}", grammar_js.display());

    let output = std::process::Command::new("tree-sitter")
        .arg("generate")
        .current_dir(parser_generator_dir)
        .output()
        .unwrap();

    if !output.status.success() {
        println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
        println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        panic!("build unsuccessful");
    }
}
