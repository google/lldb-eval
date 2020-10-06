load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")

COPTS = select({
    "@bazel_tools//src/conditions:windows": [
        # Disable warnings only for the LLVM includes.
        "/experimental:external",
        "/external:W0",
        "/external:I external/llvm_project/include",
    ],
    "//conditions:default": [
        "-fno-exceptions",
        "-fno-rtti",
    ],
})

cc_binary(
    name = "lexer",
    srcs = ["lexer.cc"],
    copts = COPTS,
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
        "fuzzer/ast_visitor.h",
        "fuzzer/expr_gen.h",
    ],
    copts = COPTS,
)

cc_binary(
    name = "fuzzer",
    srcs = ["fuzzer/main.cc"],
    copts = COPTS,
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
        "@llvm_project//:lldb-server",
    ],
    deps = [
        ":fuzzer_lib",
        "//:lldb-eval",
        "@bazel_tools//tools/cpp/runfiles",
        "@llvm_project//:lldb-api",
    ],
)
