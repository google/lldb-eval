load("@rules_fuzzing//fuzzing:cc_defs.bzl", "cc_fuzz_test")

cc_library(
  name = "fuzzed_rng",
  srcs = ["fuzzed_rng.cc"],
  hdrs = ["fuzzed_rng.h"],
  deps = ["//tools/fuzzer:fuzzer_lib"],
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
        "//lldb-eval",
        "//lldb-eval:runner",
        "@bazel_tools//tools/cpp/runfiles",
        ":fuzzed_rng",
        "//tools/fuzzer:fuzzer_lib",
    ],
)
