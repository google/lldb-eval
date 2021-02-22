load("@rules_fuzzing//fuzzing:cc_defs.bzl", "cc_fuzz_test")
load("@rules_proto//proto:defs.bzl", "proto_library")

proto_library(
    name = "expression_proto",
    srcs = ["expression.proto"],
)

cc_proto_library(
    name = "expression_cc_proto",
    deps = [":expression_proto"],
)

cc_library(
    name = "proto_to_str",
    srcs = ["proto_to_str.cc"],
    hdrs = ["proto_to_str.h"],
    deps = [":expression_cc_proto"],
)

cc_fuzz_test(
    name = "lldb_eval_fuzzer",
    data = [
        "//testdata:fuzzer_binary_gen",
        "//testdata:fuzzer_binary_srcs",
    ],
    tags = ["no-sandbox"],
    srcs = ["lldb_eval_fuzzer.cc"],
    deps = [
        ":expression_cc_proto",
        ":proto_to_str",
        "//lldb-eval",
        "//lldb-eval:runner",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_libprotobuf_mutator//:libprotobuf_mutator"
    ],
)
