# libFuzzer integration

This only works on Linux!

## Running fuzzer

```bash
bazel --config=asan-libfuzzer //experimental/protobuf:lldb_eval_fuzzer_run

# or

bazel --config=asan-libfuzzer //experimental/fuzzed_rng:lldb_eval_fuzzer_run
```

Use `--config=libfuzzer` instead of `--config=asan-libfuzzer` to disable
address sanitizer.

## Troubleshooting

If you're changing structure of proto messages, you might encounter ERRORs when
running fuzzer. Try to clear `/tmp/fuzzing` folder where corpus and artifacts
of previous runs were saved.
