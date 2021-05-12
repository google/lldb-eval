workspace(name = "lldb_eval")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "bazel_skylib",
    sha256 = "1c531376ac7e5a180e0237938a2536de0c54d93f5c278634818e0efc952dd56c",
    urls = [
        "https://github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
    ],
)

http_archive(
    name = "com_google_googletest",
    sha256 = "94c634d499558a76fa649edb13721dce6e98fb1e7018dfaeba3cd7a083945e91",
    strip_prefix = "googletest-release-1.10.0",
    urls = ["https://github.com/google/googletest/archive/release-1.10.0.zip"],
)

http_archive(
    name = "com_google_benchmark",
    sha256 = "bdefa4b03c32d1a27bd50e37ca466d8127c1688d834800c38f3c587a396188ee",
    strip_prefix = "benchmark-1.5.3",
    urls = ["https://github.com/google/benchmark/archive/v1.5.3.zip"],
)

http_archive(
    name = "io_github_yhirose_cpplinenoise",
    build_file = "//build_defs:cpp_linenoise.BUILD",
    sha256 = "9afbfd1da0e7bd48c06a6d08ef2a1c259b2edf3f6488b75a54025ab4e64b523f",
    strip_prefix = "cpp-linenoise-bc523e4b03a690cebe3b5f80a6396bcc50215a03",
    urls = ["https://github.com/yhirose/cpp-linenoise/archive/bc523e4b03a690cebe3b5f80a6396bcc50215a03.zip"],
)

load("//build_defs:repo_rules.bzl", "llvm_project_configure")

llvm_project_configure(name = "llvm_project")
