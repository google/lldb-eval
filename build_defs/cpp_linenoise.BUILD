load("@rules_cc//cc:defs.bzl","cc_library")

package(default_visibility = ["//visibility:public"])

licenses(["notice"])

exports_files(["LICENSE"])

COPTS = select({
    "//conditions:default": [
        "-fno-exceptions",
        "-fno-rtti",
    ],
})

cc_library(
    name = "cpp_linenoise",
    hdrs = ["linenoise.hpp"],
    includes = ["."],
    include_prefix = "cpp_linenoise",
)
