load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

cc_binary(
    name = "lexer",
    srcs = ["lexer.cc"],
    deps = [
        "@llvm_project//:clang-basic",
        "@llvm_project//:clang-lex",
        "@llvm_project//:llvm-support",
    ],
)

cc_library(
    name = "fuzzer_lib",
    srcs = [
        "fuzzer/ast.cc",
        "fuzzer/expr_gen.cc",
    ],
    hdrs = [
        "fuzzer/ast.h",
        "fuzzer/expr_gen.h",
    ],
)

cc_binary(
    name = "fuzzer",
    srcs = ["fuzzer/main.cc"],
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
        "@llvm_project//:lldb-server",
    ],
    deps = [
        ":fuzzer_lib",
        "//:lldb-eval",
        "@bazel_tools//tools/cpp/runfiles",
        "@io_github_yhirose_cpplinenoise//:cpp_linenoise",
        "@llvm_project//:lldb-api",
    ],
)

cc_test(
    name = "fuzzer_test",
    srcs = ["fuzzer/fuzzer_test.cc"],
    deps = [
        ":fuzzer_lib",
        "//:lldb-eval",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
        "@llvm_project//:lldb-api",
    ],
)
