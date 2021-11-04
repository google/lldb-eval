load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

package(
    default_visibility = ["//visibility:public"],
)

cc_library(
    name = "defines",
    hdrs = ["defines.h"],
)

cc_library(
    name = "lldb-eval",
    srcs = [
        "api.cc",
        "ast.cc",
        "context.cc",
        "eval.cc",
        "parser.cc",
        "parser_context.cc",
        "type.cc",
        "value.cc",
    ],
    hdrs = [
        "api.h",
        "ast.h",
        "context.h",
        "eval.h",
        "parser.h",
        "parser_context.h",
        "traits.h",
        "type.h",
        "value.h",
    ],
    deps = [
        ":defines",
        "@llvm_project//:clang-basic",
        "@llvm_project//:clang-lex",
        "@llvm_project//:lldb-api",
        "@llvm_project//:llvm-support",
    ],
)

cc_binary(
    name = "eval_benchmark",
    srcs = ["eval_benchmark.cc"],
    data = [
        "//testdata:benchmark_binary_gen",
        "//testdata:benchmark_binary_srcs",
    ],
    tags = [
        # On Linux lldb-server behaves funny in a sandbox ¯\_(ツ)_/¯. This is
        # not necessary on Windows, but "tags" attribute is not configurable
        # with select -- https://github.com/bazelbuild/bazel/issues/2971.
        "no-sandbox",
    ],
    deps = [
        ":lldb-eval",
        ":runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_benchmark//:benchmark_main",
        "@llvm_project//:lldb-api",
    ],
)

cc_test(
    name = "eval_test",
    srcs = ["eval_test.cc"],
    data = [
        "//testdata:test_binary_gen",
        "//testdata:test_binary_srcs",
    ],
    tags = [
        # On Linux lldb-server behaves funny in a sandbox ¯\_(ツ)_/¯. This is
        # not necessary on Windows, but "tags" attribute is not configurable
        # with select -- https://github.com/bazelbuild/bazel/issues/2971.
        "no-sandbox",
    ],
    deps = [
        ":lldb-eval",
        ":runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
        "@llvm_project//:lldb-api",
    ],
)

cc_test(
    name = "ub_detection_test",
    srcs = ["ub_detection_test.cc"],
    data = [
        "//testdata:ub_detection_binary_gen",
        "//testdata:ub_detection_binary_srcs",
    ],
    tags = [
        # On Linux lldb-server behaves funny in a sandbox ¯\_(ツ)_/¯. This is
        # not necessary on Windows, but "tags" attribute is not configurable
        # with select -- https://github.com/bazelbuild/bazel/issues/2971.
        "no-sandbox",
    ],
    deps = [
        ":lldb-eval",
        ":runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
        "@llvm_project//:lldb-api",
    ],
)

cc_library(
    name = "runner",
    srcs = ["runner.cc"],
    hdrs = ["runner.h"],
    data = [
        "@llvm_project//:lldb-server",
    ],
    deps = [
        "@bazel_tools//tools/cpp/runfiles",
        "@llvm_project//:lldb-api",
    ],
)
