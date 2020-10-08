load("@rules_cc//cc:defs.bzl","cc_library")

package(default_visibility = ["//visibility:public"])

licenses(["notice"])

exports_files(["LICENSE"])

CPP_LINENOISE_COPTS = select({
    "@bazel_tools//src/conditions:windows": [
        # Disable warnings for the cpp-linenoise header
        "/experimental:external",
        "/external:W0",
        "/external:I",
        "external/io_github_yhirose_cpplinenoise",
    ],
    "//conditions:default": [
        "-fno-exceptions",
        "-fno-rtti",
    ],
})

cc_library(
    name = "cpp_linenoise",
    hdrs = ["linenoise.hpp"],
    includes = ["."],
    copts = CPP_LINENOISE_COPTS,
    include_prefix = "cpp_linenoise",
)
