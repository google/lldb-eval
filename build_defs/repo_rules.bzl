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
This module configures a local repository for an llvm-project.
"""

load("@bazel_skylib//lib:paths.bzl", "paths")

def _get_llvm_build_type(ctx, llvm_path):
    if ctx.os.name.startswith("windows"):
        # There are no prebuilt binaries for Windows, so always assume static.
        return "static"

    ldd = ctx.which("ldd")
    if ldd == None:
        # No `ldd`, can't do without it.
        return "static"

    # Execute `ldd liblldb.so` to figure out whether it depends on libLLVM.so.
    result = ctx.execute([ldd, paths.join(llvm_path, "lib", "liblldb.so")])
    if result.stdout.find("libLLVM") != -1:
        return "dynamic"

    return "static"

def _impl(repository_ctx):
    llvm_path = repository_ctx.os.environ.get("LLVM_INSTALL_PATH")
    if llvm_path == None:
        fail(
            "Error: LLVM_INSTALL_PATH is not defined. Set it to a location " +
            "with LLVM {bin,lib,include} directories. E.g. \"/usr/lib/llvm\".",
        )

    # Try to guess a default value for :llvm_build flag. It should be "dynamic"
    # if liblldb.so is built dynamically and "static" otherwise.
    llvm_build = _get_llvm_build_type(repository_ctx, llvm_path)

    repository_ctx.symlink(paths.join(llvm_path, "bin"), "bin")
    repository_ctx.symlink(paths.join(llvm_path, "lib"), "lib")
    repository_ctx.symlink(paths.join(llvm_path, "include"), "include")
    repository_ctx.template(
        "BUILD",
        Label("//build_defs:llvm_project_local.BUILD"),
        substitutions = {"{LLVM_BUILD_DEFAULT}": llvm_build},
    )

llvm_project_configure = repository_rule(
    implementation = _impl,
    environ = ["LLVM_INSTALL_PATH"],
    local = True,
)
