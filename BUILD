"""
To build on Windows don't forget to set BAZEL_VS env variable. See more at
https://docs.bazel.build/versions/master/windows.html#build-c-with-msvc.
"""

load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

package(
    default_visibility = ["//visibility:public"],
)

licenses(["notice"])

exports_files(["LICENSE"])

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

cc_library(
    name = "lldb-eval",
    srcs = [
        "src/api.cc",
        "src/ast.cc",
        "src/eval.cc",
        "src/expression_context.cc",
        "src/parser.cc",
        "src/pointer.cc",
        "src/scalar.cc",
        "src/value.cc",
    ],
    hdrs = [
        "src/api.h",
        "src/ast.h",
        "src/defines.h",
        "src/eval.h",
        "src/expression_context.h",
        "src/parser.h",
        "src/pointer.h",
        "src/scalar.h",
        "src/value.h",
    ],
    copts = COPTS,
    deps = [
        "@llvm_project_local//:clang-basic",
        "@llvm_project_local//:clang-lex",
        "@llvm_project_local//:lldb-api",
        "@llvm_project_local//:llvm-support",
    ],
)

cc_test(
    name = "eval_test",
    srcs = ["src/eval_test.cc"],
    copts = COPTS,
    tags = [
        # On Linux lldb-server behaves funny in a sandbox ¯\_(ツ)_/¯. This is
        # not necessary on Windows, but "tags" attribute is not configurable
        # with select -- https://github.com/bazelbuild/bazel/issues/2971.
        "no-sandbox",
    ],
    deps = [
        ":lldb-eval",
        ":runner",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
        "@llvm_project_local//:lldb-api",
    ],
)

cc_binary(
    name = "main",
    srcs = ["src/main.cc"],
    copts = COPTS,
    deps = [
        ":lldb-eval",
        ":runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@llvm_project_local//:lldb-api",
    ],
)

cc_test(
    name = "parser_test",
    srcs = ["src/parser_test.cc"],
    copts = COPTS,
    deps = [
        ":lldb-eval",
        "@com_google_googletest//:gtest",
        "@com_google_googletest//:gtest_main",
        "@llvm_project_local//:lldb-api",
    ],
)

cc_library(
    name = "runner",
    srcs = ["src/runner.cc"],
    hdrs = ["src/runner.h"],
    copts = COPTS,
    data = [
        "//testdata:test_binary_gen",
        "//testdata:test_binary_srcs",
    ] + select({
        "@bazel_tools//src/conditions:windows": [
            # There is no lldb-server on Windows.
        ],
        "//conditions:default": [
            "@llvm_project_local//:lldb-server",
        ],
    }),
    deps = [
        "@bazel_tools//tools/cpp/runfiles",
        "@llvm_project_local//:lldb-api",
    ],
)
