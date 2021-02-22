load("@rules_cc//cc:defs.bzl","cc_library")

package(default_visibility = ["//visibility:public"])

licenses(["notice"])

exports_files(["LICENSE"])

cc_library(
    name = "libprotobuf_mutator",
    srcs = glob(
        [
            "src/**/*.cc",
            "src/**/*.h",
            "port/protobuf.h",
        ],
        exclude = ["**/*_test.cc"],
    ),
    hdrs = ["src/libfuzzer/libfuzzer_macro.h"],
    include_prefix = "libprotobuf_mutator",
    includes = ["."],
    deps = ["@com_google_protobuf//:protobuf"],
)
