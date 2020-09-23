load("@rules_cc//cc:defs.bzl", "cc_library")

package(default_visibility = ["//visibility:public"])

LLVM_LINKOPTS = select({
    "@bazel_tools//src/conditions:windows": [],
    "//conditions:default": [
        # System libraries: $(llvm-config --system-libs)
        "-lz",
        "-lrt",
        "-ldl",
        "-ltinfo",
        "-lpthread",
        "-lm",
    ],
})

filegroup(
    name = "files",
    srcs = glob(["llvm/**"]),
)

filegroup(
    name = "lldb-server",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            # There is no lldb-server on Windows.
        ],
        "//conditions:default": [
            "bin/lldb-server",
        ],
    }),
)

filegroup(
    name = "clang",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "bin/clang.exe",
        ],
        "//conditions:default": [
            "bin/clang",
        ],
    }),
)

filegroup(
    name = "lld",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "bin/lld.exe",
        ],
        "//conditions:default": [
            "bin/lld",
        ],
    }),
)

filegroup(
    name = "llvm-headers",
    srcs = glob([
        "include/llvm/**/*.h",
        "include/llvm/**/*.def",
        "include/llvm/**/*.inc",
        "include/llvm-c/**/*.h",
        "include/llvm-c/**/*.def",
        "include/llvm-c/**/*.inc",
    ]),
)

filegroup(
    name = "clang-headers",
    srcs = glob([
        "include/clang/**/*.h",
        "include/clang/**/*.def",
        "include/clang/**/*.inc",
        "include/clang-c/**/*.h",
        "include/clang-c/**/*.def",
        "include/clang-c/**/*.inc",
    ]),
)

cc_library(
    name = "llvm-support",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "lib/LLVMSupport.lib",
            "lib/LLVMDemangle.lib",
        ],
        "//conditions:default": [
            "lib/libLLVMSupport.a",
            "lib/libLLVMDemangle.a",
        ],
    }),
    hdrs = [":llvm-headers"],
    includes = ["include"],
    linkopts = LLVM_LINKOPTS,
)

cc_library(
    name = "llvm-core",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            # $(llvm-config --libs mc core support)
            "lib/LLVMCore.lib",
            "lib/LLVMRemarks.lib",
            "lib/LLVMBitstreamReader.lib",
            "lib/LLVMBinaryFormat.lib",
        ],
        "//conditions:default": [
            # $(llvm-config --libs mc core support)
            "lib/libLLVMCore.a",
            "lib/libLLVMRemarks.a",
            "lib/libLLVMBitstreamReader.a",
            "lib/libLLVMBinaryFormat.a",
        ],
    }),
    hdrs = [":llvm-headers"],
    includes = ["include"],
    deps = [":llvm-support"],
)

cc_library(
    name = "llvm-mc",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "lib/LLVMMC.lib",
            "lib/LLVMDebugInfoCodeView.lib",
            "lib/LLVMDebugInfoMSF.lib",
        ],
        "//conditions:default": [
            "lib/libLLVMMC.a",
            "lib/libLLVMDebugInfoCodeView.a",
            "lib/libLLVMDebugInfoMSF.a",
        ],
    }),
    hdrs = [":llvm-headers"],
    includes = ["include"],
    deps = [":llvm-core"],
)

cc_library(
    name = "llvm-shared",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
        ],
        "//conditions:default": glob([
            "lib/libLLVM*.so",
        ]),
    }),
    hdrs = [":llvm-headers"],
    includes = ["include"],
    deps = [
        ":llvm-core",
        ":llvm-mc",
        ":llvm-support",
    ],
)

cc_library(
    name = "clang-basic",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "lib/clangBasic.lib",
        ],
        "//conditions:default": [
            "lib/libclangBasic.a",
        ],
    }),
    hdrs = [":clang-headers"],
    includes = ["include"],
    deps = [
        ":llvm-core",
        ":llvm-mc",
        ":llvm-support",
    ],
)

cc_library(
    name = "clang-lex",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "lib/clangLex.lib",
        ],
        "//conditions:default": [
            "lib/libclangLex.a",
        ],
    }),
    hdrs = [":clang-headers"],
    includes = ["include"],
    deps = [
        ":clang-basic",
        ":llvm-support",
    ],
)

cc_library(
    name = "lldb-api",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "bin/liblldb.dll",
            "lib/liblldb.lib",
        ],
        "//conditions:default": glob([
            "lib/liblldb.so*",
        ]),
    }),
    hdrs = glob([
        "include/lldb/**/*.h",
        "include/lldb/**/*.def",
        "include/lldb/**/*.inc",
    ]),
    includes = ["include"],
    deps = [
        ":llvm-shared",  # liblldb can be dynamically linked and depend on libLLVM.
        ":llvm-support",
    ],
)
