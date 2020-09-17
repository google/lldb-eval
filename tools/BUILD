load("@rules_cc//cc:defs.bzl", "cc_binary")

COPTS = select({
    "@bazel_tools//src/conditions:windows": [
        # Disable warnings only for the LLVM includes.
        "/experimental:external",
        "/external:W0",
        "/external:I external/llvm_project_local/include",
    ],
    "//conditions:default": [
        "-std=c++14",
        "-fno-exceptions",
        "-fno-rtti",
    ],
})

cc_binary(
    name = "lexer",
    srcs = ["lexer.cc"],
    copts = COPTS,
    deps = [
        "@llvm_project_local//:clang-basic",
        "@llvm_project_local//:clang-lex",
        "@llvm_project_local//:llvm-support",
    ],
)
