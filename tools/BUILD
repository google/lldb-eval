load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "exec",
    srcs = ["exec.cc"],
    deps = [
        "//lldb-eval",
        "//lldb-eval:runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@io_github_yhirose_cpplinenoise//:cpp_linenoise",
        "@llvm_project//:lldb-api",
    ],
)

cc_binary(
    name = "lexer",
    srcs = ["lexer.cc"],
    deps = [
        "@llvm_project//:clang-basic",
        "@llvm_project//:clang-lex",
        "@llvm_project//:llvm-support",
    ],
)
