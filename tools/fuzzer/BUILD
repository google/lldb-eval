load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")
load("@rules_fuzzing//fuzzing:cc_defs.bzl", "cc_fuzz_test")

cc_library(
    name = "fuzzer_lib",
    srcs = [
        "ast.cc",
        "constraints.cc",
        "expr_gen.cc",
        "fixed_rng.cc",
        "gen_node.cc",
        "symbol_table.cc",
    ],
    hdrs = [
        "ast.h",
        "constraints.h",
        "enum_bitset.h",
        "expr_gen.h",
        "fixed_rng.h",
        "gen_node.h",
        "libfuzzer_utils.h",
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
    name = "libfuzzer_utils_test",
    srcs = ["libfuzzer_utils_test.cc"],
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
        "@llvm_project//:lldb-api",
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
        "@llvm_project//:lldb-api",
    ],
)

cc_library(
    name = "libfuzzer_common",
    srcs = ["libfuzzer_common.cc"],
    hdrs = ["libfuzzer_common.h"],
    deps = [
        ":fuzzer_lib",
        "//lldb-eval",
        "//lldb-eval:runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@llvm_project//:lldb-api",
    ],
)

cc_fuzz_test(
    name = "lldb_eval_libfuzzer_test",
    srcs = ["lldb_eval_libfuzzer_test.cc"],
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
    ],
    tags = [
        "manual",
        # On Linux lldb-server behaves funny in a sandbox ¯\_(ツ)_/¯. This is
        # not necessary on Windows, but "tags" attribute is not configurable
        # with select -- https://github.com/bazelbuild/bazel/issues/2971.
        "no-sandbox",
    ],
    # cc_fuzz_test doesn't work well on Windows.
    target_compatible_with = ["@platforms//os:linux"],
    deps = [
        ":fuzzer_lib",
        ":libfuzzer_common",
        "//lldb-eval",
        "@llvm_project//:lldb-api",
    ],
)

cc_fuzz_test(
    name = "lldb_vs_lldb_eval_libfuzzer_test",
    srcs = ["lldb_vs_lldb_eval_libfuzzer_test.cc"],
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
    ],
    tags = [
        "manual",
        # On Linux lldb-server behaves funny in a sandbox ¯\_(ツ)_/¯. This is
        # not necessary on Windows, but "tags" attribute is not configurable
        # with select -- https://github.com/bazelbuild/bazel/issues/2971.
        "no-sandbox",
    ],
    # cc_fuzz_test doesn't work well on Windows.
    target_compatible_with = ["@platforms//os:linux"],
    deps = [
        ":fuzzer_lib",
        ":libfuzzer_common",
        "//lldb-eval",
        "@llvm_project//:lldb-api",
    ],
)
