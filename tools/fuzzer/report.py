#!/usr/bin/env python3
# Copyright 2021 Google LLC
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
""" Processes libfuzzer log files to filter logs coming from lldb-eval-fuzzer. 

    Usage examples:
        python report.py logs/*/*.log
        python report.py logs/2021-07-{20,21}/*.log
        python report.py --disable-ub-stats *.log
        python report.py --ub-stats *.log
"""

import argparse
import collections
import glob


FUZZER_LOGGING_PREFIX = "[lldb-eval-fuzzer] "
UB_STATS_PREFIX = "UB stats:"


class LogsProcessor:
  _skip_line = False
  _disable_ub_stats = False

  def __init__(self, disable_ub_stats):
    self._disable_ub_stats = disable_ub_stats

  def process_line(self, line):
    if self._skip_line:
      self._skip_line = False
      return

    if self._disable_ub_stats and line.startswith(UB_STATS_PREFIX):
      self._skip_line = True
      return

    print(line, end="")


class UbStatsProcessor:
  _current_file_stats = collections.Counter()
  _ub_stats = collections.Counter()

  def process_line(self, line):
    if not line.startswith(UB_STATS_PREFIX):
      return

    start = line.find("(") + 1
    end = line.find(")")

    # We're interested in the last UB stats log in the file.
    # Reset it each time it is encountered in the same file.
    self._current_file_stats = collections.Counter()

    for pair in line[start:end].split(", "):
      key, value = pair.split(": ")
      self._current_file_stats[key] = int(value)

  def end_of_file(self):
    self._ub_stats += self._current_file_stats

  def ub_stats(self):
    return self._ub_stats


def read_fuzzer_log_lines(files, processor):
  for filename in files:
    with open(filename) as f:
      for line in f:
        if line.startswith(FUZZER_LOGGING_PREFIX):
          processor.process_line(line[len(FUZZER_LOGGING_PREFIX):])
    if hasattr(processor, "end_of_file"):
      processor.end_of_file()


def print_ub_stats(stats):
  if not stats:
    print("No UB stats to report")
    return
  values_sum = sum(stats.values())
  max_key_len = max(map(len, list(stats)))
  max_val_len = max(map(len, map(str, stats.values())))
  for key in stats:
    percent = float(stats[key]) * 100 / values_sum
    print(key.ljust(max_key_len), ": ", end="")
    print(str(stats[key]).rjust(max_val_len), end="")
    print(" (%.1f%%)" % percent)


if __name__ == "__main__":
  parser = argparse.ArgumentParser()
  parser.add_argument("--disable-ub-stats", action="store_true",
                      help="discard undefined behaviour logs")
  parser.add_argument("--ub-stats", action="store_true",
                      help="show only stats on undefined behaviour")

  # Parse argument options. The remaining arguments are file patterns.
  args, file_patterns = parser.parse_known_args()

  assert not args.disable_ub_stats or not args.ub_stats, "Wrong configuration!"

  files = []
  for pattern in file_patterns:
    for filename in glob.glob(pattern):
      files.append(filename)

  if args.ub_stats:
    processor = UbStatsProcessor()
    read_fuzzer_log_lines(files, processor)
    print_ub_stats(processor.ub_stats())
  else:
    processor = LogsProcessor(args.disable_ub_stats)
    read_fuzzer_log_lines(files, processor)
