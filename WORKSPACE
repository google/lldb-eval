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
    name = "io_github_yhirose_cpplinenoise",
    build_file = "//build_defs:cpp_linenoise.BUILD",
    sha256 = "9afbfd1da0e7bd48c06a6d08ef2a1c259b2edf3f6488b75a54025ab4e64b523f",
    strip_prefix = "cpp-linenoise-bc523e4b03a690cebe3b5f80a6396bcc50215a03",
    urls = ["https://github.com/yhirose/cpp-linenoise/archive/bc523e4b03a690cebe3b5f80a6396bcc50215a03.zip"],
)

http_archive(
    name = "rules_fuzzing",
    sha256 = "71fa2724c9802c597199a86111a0499fc4fb22426d322334d3f191dadeff5132",
    strip_prefix = "rules_fuzzing-0.1.0",
    urls = ["https://github.com/bazelbuild/rules_fuzzing/archive/v0.1.0.zip"],
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "6dd0f6b20094910fbb7f1f7908688df01af2d4f6c5c21331b9f636048674aebf",
    strip_prefix = "protobuf-3.14.0",
    urls = ["https://github.com/protocolbuffers/protobuf/releases/download/v3.14.0/protobuf-all-3.14.0.tar.gz"],
)

http_archive(
    name = "com_google_libprotobuf_mutator",
    build_file = "//build_defs:libprotobuf_mutator.BUILD",
    sha256 =  "e4c66f7890b1aeab756960106be9081e001327fd6af798ddff65cc3ab5f6f847",
    strip_prefix = "libprotobuf-mutator-1.0",
    urls = ["https://github.com/google/libprotobuf-mutator/archive/v1.0.zip"],
)


load("@rules_fuzzing//fuzzing:repositories.bzl", "rules_fuzzing_dependencies")
rules_fuzzing_dependencies()
load("@rules_fuzzing//fuzzing:init.bzl", "rules_fuzzing_init")
rules_fuzzing_init()

load("//build_defs:repo_rules.bzl", "llvm_project_configure")

llvm_project_configure(name = "llvm_project")




load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")
protobuf_deps()


