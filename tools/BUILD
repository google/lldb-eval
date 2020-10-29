load("@rules_cc//cc:defs.bzl", "cc_binary")

COPTS = select({
    "@bazel_tools//src/conditions:windows": [
        "/external:I external/llvm_project/include",
    ],
    "//conditions:default": [],
})

cc_binary(
    name = "exec",
    srcs = ["exec.cc"],
    copts = COPTS + select({
        "@bazel_tools//src/conditions:windows": [
            "/external:I $(BINDIR)/external/io_github_yhirose_cpplinenoise",
        ],
        "//conditions:default": [],
    }),
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
    copts = COPTS,
    deps = [
        "@llvm_project//:clang-basic",
        "@llvm_project//:clang-lex",
        "@llvm_project//:llvm-support",
    ],
)
