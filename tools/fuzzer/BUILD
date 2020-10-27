load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

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
    deps = [
        "//lldb-eval:defines",
    ],
)

cc_binary(
    name = "fuzzer",
    srcs = ["main.cc"],
    copts = select({
        "@bazel_tools//src/conditions:windows": [
            # Disable warnings for llvm_project
            "/external:I external/llvm_project/include",
            # Disable warnings for cpp-linenoise
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
        "//lldb-eval:runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@io_github_yhirose_cpplinenoise//:cpp_linenoise",
        "@llvm_project//:lldb-api",
    ],
)

cc_test(
    name = "fuzzer_test",
    srcs = ["fuzzer_test.cc"],
    deps = [
        ":fuzzer_lib",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
    ],
)
