# Copyright 2020 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
Rules for building test binaries.
"""

def binary_gen(name, srcs, use_libcxx = False):
    native.filegroup(
        name = name + "_srcs",
        srcs = srcs,
    )

    build_cmd = """
        $(location @llvm_project//:clang) \
        -x c++ -std=c++17 -O0 -gdwarf -fuse-ld=lld \
        {platform_opts} \
        $(SRCS) -o $@
    """

    native.genrule(
        name = name + "_gen",
        srcs = srcs,
        outs = [name],
        cmd = select({
            "@bazel_tools//src/conditions:windows": build_cmd.format(
                platform_opts = "--for-linker -debug:dwarf",
            ),
            "//conditions:default": build_cmd.format(
                # Using "-static" prevents fuzzer_binary from using __log2
                # function from libm.so.
                platform_opts = "-stdlib=libc++ -lc++" if use_libcxx else "-lstdc++ -static",
            ),
        }),
        tags = ["no-sandbox"],
        tools = [
            "@llvm_project//:clang",
            "@llvm_project//:lld",
        ],
    )
