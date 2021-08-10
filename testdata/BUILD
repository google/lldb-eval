load("rules.bzl", "binary_gen")

package(default_visibility = ["//visibility:public"])

binary_gen(
    name = "benchmark_binary",
    srcs = [
        "benchmark_binary.cc",
    ],
)

binary_gen(
    name = "fuzzer_binary",
    srcs = [
        "fuzzer_binary.cc",
    ],
)

binary_gen(
    name = "test_binary",
    srcs = [
        "test_binary.cc",
        "test_library.cc",
    ],
    use_libcxx = True,
)

binary_gen(
    name = "ub_detection_binary",
    srcs = [
        "ub_detection_binary.cc",
    ],
)
