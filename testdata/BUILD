load("rules.bzl", "binary_gen")

package(default_visibility = ["//visibility:public"])

binary_gen(
    name = "test_binary",
    srcs = [
        "test_binary.cc",
        "test_library.cc",
    ],
)

binary_gen(
    name = "fuzzer_binary",
    srcs = [
        "fuzzer_binary.cc",
    ],
)
