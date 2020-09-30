load("@rules_cc//cc:defs.bzl", "cc_binary")

COPTS = select({
    "@bazel_tools//src/conditions:windows": [
        # Disable warnings only for the LLVM includes.
        "/experimental:external",
        "/external:W0",
        "/external:I external/llvm_project_local/include",
    ],
    "//conditions:default": [
        "-std=c++17",
        "-fno-exceptions",
        "-fno-rtti",
    ],
})

cc_binary(
    name = "lexer",
    srcs = ["lexer.cc"],
    copts = COPTS,
    deps = [
        "@llvm_project_local//:clang-basic",
        "@llvm_project_local//:clang-lex",
        "@llvm_project_local//:llvm-support",
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
    deps = [
        "//tools:fuzzer_lib",
        "//:lldb-eval",
        "@bazel_tools//tools/cpp/runfiles",
        "@llvm_project_local//:lldb-api",
    ],
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
    ] + select({
        "@bazel_tools//src/conditions:windows": [
            # There is no lldb-server on Windows.
        ],
        "//conditions:default": [
            "@llvm_project_local//:lldb-server",
        ],
    }),
)
