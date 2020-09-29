package(default_visibility = ["//visibility:public"])

genrule(
    name = "test_binary_gen",
    srcs = [
        "test_binary.cc",
        "test_library.cc",
    ],
    outs = ["test_binary"],
    cmd = """
        ./$(location @llvm_project_local//:clang) \
        -x c++ -lstdc++ -std=c++14 -gdwarf -O0 -fuse-ld=lld \
        $(SRCS) -o $@
    """,
    tags = ["no-sandbox"],
    tools = [
        "@llvm_project_local//:clang",
        "@llvm_project_local//:lld",
    ],
)

filegroup(
    name = "test_binary_srcs",
    srcs = [
        "test_binary.cc",
        "test_library.cc",
    ],
)

genrule(
    name = "fuzzer_binary_gen",
    srcs = [
        "fuzzer_binary.cc",
    ],
    outs = ["fuzzer_binary"],
    cmd = """
        ./$(location @llvm_project_local//:clang) \
        -x c++ -lstdc++ -std=c++14 -gdwarf -O0 -fuse-ld=lld \
        $(SRCS) -o $@
    """,
    tags = ["no-sandbox"],
    tools = [
        "@llvm_project_local//:clang",
        "@llvm_project_local//:lld",
    ],
)

filegroup(
    name = "fuzzer_binary_srcs",
    srcs = [
        "fuzzer_binary.cc",
    ],
)
