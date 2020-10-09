load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

COPTS = select({
    "@bazel_tools//src/conditions:windows": [
        # Mark the LLVM includes as external
        "/external:I external/llvm_project/include",
    ],
    "//conditions:default": [],
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
        "fuzzer/expr_gen.h",
    ],
    include_prefix = "fuzzer",
    strip_include_prefix = "fuzzer",
)

cc_binary(
    name = "fuzzer",
    srcs = ["fuzzer/main.cc"],
    copts = COPTS + select({
        "@bazel_tools//src/conditions:windows": [
            # Disable warnings only for cpp-linenoise
            "/external:I $(BINDIR)/external/io_github_yhirose_cpplinenoise/_virtual_includes",
        ],
        "//conditions:default": [],
    }),
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
    copts = COPTS,
    deps = [
        ":fuzzer_lib",
        "//:lldb-eval",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
        "@llvm_project//:lldb-api",
    ],
)
