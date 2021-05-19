/*
 * Copyright 2021 Google LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef INCLUDE_RNG_H
#define INCLUDE_RNG_H

#include <cassert>
#include <functional>
#include <queue>
#include <random>

namespace fuzzer {

typedef std::mt19937::result_type rand_t;

// Extracting common logic for calling a callback with the generated value.
class RngCallbackNotifier {
 public:
  void set_callback(std::function<void(rand_t)> callback) {
    callback_ = std::move(callback);
  }

 protected:
  rand_t consume(rand_t value) {
    callback_(value);
    return value;
  }

 private:
  std::function<void(rand_t)> callback_;
};

class Mt19937 : public RngCallbackNotifier {
 public:
  explicit Mt19937(rand_t seed) : rng_(seed) {}

  using result_type = rand_t;
  static constexpr result_type min() { return std::mt19937::min(); }
  static constexpr result_type max() { return std::mt19937::max(); }

  result_type operator()() { return consume(rng_()); }

 private:
  std::mt19937 rng_;
};

// Random number generator over fixed number sequence.
class FixedRng : public RngCallbackNotifier {
 public:
  explicit FixedRng(std::queue<rand_t> q) : queue_(std::move(q)) {}

  using result_type = rand_t;
  static constexpr result_type min() { return std::mt19937::min(); }
  static constexpr result_type max() { return std::mt19937::max(); }

  result_type operator()() {
    assert(!queue_.empty());
    auto value = queue_.front();
    queue_.pop();
    return consume(value);
  }

 private:
  std::queue<rand_t> queue_;
};

// Generates a sequence of `rand_t` values from a byte sequence provided by
// libFuzzer. It assumes that the byte sequence is infinite with all bytes
// after the specified size being equal to zero.
class LibfuzzerReader {
 public:
  LibfuzzerReader(const uint8_t* data, size_t size)
      : data_(data), size_(size), cursor_(0) {}

  rand_t read() {
    rand_t result = next_byte();
    result <<= 8;
    result |= next_byte();
    result <<= 8;
    result |= next_byte();
    result <<= 8;
    result |= next_byte();
    return result;
  }

 private:
  uint8_t next_byte() { return cursor_ < size_ ? data_[cursor_++] : 0; }

 private:
  const uint8_t* data_;
  size_t size_;
  size_t cursor_;
};

// Converts a sequence of `rand_t` values to byte sequence.
class LibfuzzerWriter {
 public:
  LibfuzzerWriter(uint8_t* data, size_t max_size)
      : data_(data), max_size_(max_size), size_(0) {}

  void write(rand_t value) {
    write_byte((value >> 24) & 0xff);
    write_byte((value >> 16) & 0xff);
    write_byte((value >> 8) & 0xff);
    write_byte(value & 0xff);
  }

  size_t size() const { return size_; }

 private:
  void write_byte(uint8_t byte) {
    if (size_ < max_size_) {
      data_[size_++] = byte;
    }
  }

 private:
  uint8_t* data_;
  size_t max_size_;
  size_t size_;
};

class LibfuzzerRng : public RngCallbackNotifier {
 public:
  explicit LibfuzzerRng(LibfuzzerReader reader) : reader_(std::move(reader)) {}

  using result_type = rand_t;
  static constexpr result_type min() { return std::mt19937::min(); }
  static constexpr result_type max() { return std::mt19937::max(); }

  result_type operator()() { return consume(reader_.read()); }

 private:
  LibfuzzerReader reader_;
};

}  // namespace fuzzer

#endif  // INCLUDE_RNG_H
