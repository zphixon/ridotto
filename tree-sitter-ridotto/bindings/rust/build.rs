fn main() {
    let parser_generator_dir =
        std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap())
            .parent()
            .unwrap()
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

    let src_dir = std::path::Path::new("src");

    let mut c_config = cc::Build::new();
    c_config.include(&src_dir);
    c_config
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable")
        .flag_if_supported("-Wno-trigraphs");
    let parser_path = src_dir.join("parser.c");
    c_config.file(&parser_path);

    // If your language uses an external scanner written in C,
    // then include this block of code:

    /*
    let scanner_path = src_dir.join("scanner.c");
    c_config.file(&scanner_path);
    println!("cargo:rerun-if-changed={}", scanner_path.to_str().unwrap());
    */

    c_config.compile("parser");

    // If your language uses an external scanner written in C++,
    // then include this block of code:

    /*
    let mut cpp_config = cc::Build::new();
    cpp_config.cpp(true);
    cpp_config.include(&src_dir);
    cpp_config
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable");
    let scanner_path = src_dir.join("scanner.cc");
    cpp_config.file(&scanner_path);
    cpp_config.compile("scanner");
    println!("cargo:rerun-if-changed={}", scanner_path.to_str().unwrap());
    */
}
