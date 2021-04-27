load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

cc_library(
    name = "fuzzer_lib",
    srcs = [
        "ast.cc",
        "constraints.cc",
        "expr_gen.cc",
        "gen_node.cc",
        "symbol_table.cc",
    ],
    hdrs = [
        "ast.h",
        "constraints.h",
        "enum_bitset.h",
        "expr_gen.h",
        "gen_node.h",
        "rng.h",
        "symbol_table.h",
    ],
    deps = [
        "//lldb-eval",
        "@llvm_project//:lldb-api",
    ],
)

cc_binary(
    name = "fuzzer",
    srcs = ["main.cc"],
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
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
    name = "constraints_test",
    srcs = ["constraints_test.cc"],
    deps = [
        ":fuzzer_lib",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
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

cc_test(
    name = "symbol_table_test",
    srcs = ["symbol_table_test.cc"],
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
    ],
    tags = [
        # On Linux lldb-server behaves funny in a sandbox ¯\_(ツ)_/¯. This is
        # not necessary on Windows, but "tags" attribute is not configurable
        # with select -- https://github.com/bazelbuild/bazel/issues/2971.
        "no-sandbox",
    ],
    deps = [
        ":fuzzer_lib",
        "//lldb-eval",
        "//lldb-eval:runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "mutate_test",
    srcs = ["mutate_test.cc"],
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
    ],
    tags = [
        # On Linux lldb-server behaves funny in a sandbox ¯\_(ツ)_/¯. This is
        # not necessary on Windows, but "tags" attribute is not configurable
        # with select -- https://github.com/bazelbuild/bazel/issues/2971.
        "no-sandbox",
    ],
    deps = [
        ":fuzzer_lib",
        "//lldb-eval",
        "//lldb-eval:runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
    ],
)
