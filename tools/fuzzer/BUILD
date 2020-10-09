load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

COPTS = select({
    "@bazel_tools//src/conditions:windows": [
        # Mark the LLVM includes as external
        "/external:I external/llvm_project/include",
    ],
    "//conditions:default": [],
})

cc_library(
    name = "fuzzer_lib",
    srcs = [
        "ast.cc",
        "expr_gen.cc",
    ],
    hdrs = [
        "ast.h",
        "expr_gen.h",
    ],
)

cc_binary(
    name = "fuzzer",
    srcs = ["main.cc"],
    copts = COPTS + select({
        "@bazel_tools//src/conditions:windows": [
            # Disable warnings only for cpp-linenoise
            "/external:I $(BINDIR)/external/io_github_yhirose_cpplinenoise",
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
        "//lldb-eval",
        "@bazel_tools//tools/cpp/runfiles",
        "@io_github_yhirose_cpplinenoise//:cpp_linenoise",
        "@llvm_project//:lldb-api",
    ],
)

cc_test(
    name = "fuzzer_test",
    srcs = ["fuzzer_test.cc"],
    copts = COPTS,
    deps = [
        ":fuzzer_lib",
        "//lldb-eval",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
        "@llvm_project//:lldb-api",
    ],
)
