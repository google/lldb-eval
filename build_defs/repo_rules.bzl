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

def _impl(repository_ctx):
    llvm_path = repository_ctx.os.environ.get("LLVM_INSTALL_PATH")
    if llvm_path == None:
        fail(
            "Error: LLVM_INSTALL_PATH is not defined. Set it to a location " +
            "with LLVM {bin,lib,include} directories. E.g. \"/usr/lib/llvm\".",
        )

    repository_ctx.symlink(llvm_path + "/bin", "bin")
    repository_ctx.symlink(llvm_path + "/lib", "lib")
    repository_ctx.symlink(llvm_path + "/include", "include")
    repository_ctx.template(
        "BUILD",
        Label("//build_defs:llvm_project_local.BUILD"),
    )

llvm_project_configure = repository_rule(
    implementation = _impl,
    environ = ["LLVM_INSTALL_PATH"],
)
