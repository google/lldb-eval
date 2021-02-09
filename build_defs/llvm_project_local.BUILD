load("@bazel_skylib//lib:selects.bzl", "selects")
load("@bazel_skylib//rules:common_settings.bzl", "string_flag")
load("@rules_cc//cc:defs.bzl", "cc_library")

package(default_visibility = ["//visibility:public"])

string_flag(
    name = "llvm_build",
    build_setting_default = "{LLVM_BUILD_DEFAULT}",
    values = [
        "static",
        "dynamic",
    ],
)

config_setting(
    name = "llvm_build_static",
    flag_values = {":llvm_build": "static"},
)

config_setting(
    name = "llvm_build_dynamic",
    flag_values = {":llvm_build": "dynamic"},
)

selects.config_setting_group(
    name = "linux_dynamic",
    match_all = [
        "@bazel_tools//src/conditions:linux_x86_64",
        ":llvm_build_dynamic",
    ],
)

selects.config_setting_group(
    name = "linux_static",
    match_all = [
        "@bazel_tools//src/conditions:linux_x86_64",
        ":llvm_build_static",
    ],
)

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
    name = "libllvm-so",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
        ],
        "//conditions:default": glob([
            "lib/libLLVM*.so*",
        ]),
    }),
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

filegroup(
    name = "libclang-cpp-so",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
        ],
        "//conditions:default": glob([
            "lib/libclang-cpp*.so*",
        ]),
    }),
)

cc_library(
    name = "llvm-support",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "lib/LLVMSupport.lib",
            "lib/LLVMDemangle.lib",
        ],
        ":linux_dynamic": [
            ":libllvm-so",
        ],
        ":linux_static": [
            "lib/libLLVMSupport.a",
            "lib/libLLVMDemangle.a",
        ],
    }),
    hdrs = [":llvm-headers"],
    includes = ["include"],
    linkopts = LLVM_LINKOPTS,
    linkstatic = 1,
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
        ":linux_dynamic": [
            ":libllvm-so",
        ],
        ":linux_static": [
            "lib/libLLVMCore.a",
            "lib/libLLVMRemarks.a",
            "lib/libLLVMBitstreamReader.a",
            "lib/libLLVMBinaryFormat.a",
        ],
    }),
    hdrs = [":llvm-headers"],
    includes = ["include"],
    linkstatic = 1,
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
        ":linux_dynamic": [
            ":libllvm-so",
        ],
        ":linux_static": [
            "lib/libLLVMMC.a",
            "lib/libLLVMDebugInfoCodeView.a",
            "lib/libLLVMDebugInfoMSF.a",
        ],
    }),
    hdrs = [":llvm-headers"],
    includes = ["include"],
    linkstatic = 1,
    deps = [":llvm-core"],
)

cc_library(
    name = "llvm-shared",
    srcs = [":libllvm-so"],
    hdrs = [":llvm-headers"],
    includes = ["include"],
    linkstatic = 1,
)

cc_library(
    name = "clang-basic",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "lib/clangBasic.lib",
        ],
        ":linux_dynamic": [
            ":libclang-cpp-so",
        ],
        ":linux_static": [
            "lib/libclangBasic.a",
        ],
    }),
    hdrs = [":clang-headers"],
    includes = ["include"],
    linkstatic = 1,
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
        ":linux_dynamic": [
            ":libclang-cpp-so",
        ],
        ":linux_static": [
            "lib/libclangLex.a",
        ],
    }),
    hdrs = [":clang-headers"],
    includes = ["include"],
    linkstatic = 1,
    deps = [
        ":clang-basic",
        ":llvm-support",
    ],
)

cc_library(
    name = "clangcpp-shared",
    srcs = [":libclang-cpp-so"],
    hdrs = [":clang-headers"],
    includes = ["include"],
    linkstatic = 1,
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
    defines = select({
        "@bazel_tools//src/conditions:windows": [
            "IMPORT_LIBLLDB",
        ],
        "//conditions:default": [],
    }),
    includes = ["include"],
    deps = select({
        ":llvm_build_dynamic": [
            ":llvm-shared",
            ":clangcpp-shared",
        ],
        ":llvm_build_static": [
            # Don't have any dependencies if linked statically.
        ],
    }),
)
