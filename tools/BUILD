load("@rules_cc//cc:defs.bzl", "cc_binary")

COPTS = select({
    "@bazel_tools//src/conditions:windows": [
        # Mark the LLVM includes as external
        "/external:I external/llvm_project/include",
    ],
    "//conditions:default": [],
})

cc_binary(
    name = "exec",
    srcs = ["exec.cc"],
    copts = COPTS,
    deps = [
        "//lldb-eval",
        "//lldb-eval:runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@llvm_project//:lldb-api",
    ],
)

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
