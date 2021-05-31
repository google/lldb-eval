// Copyright 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef INCLUDE_LIBFUZZER_UTILS_H
#define INCLUDE_LIBFUZZER_UTILS_H

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <type_traits>

namespace fuzzer {

// A class that consumes byte sequence provided by libFuzzer and produces a
// sequence of meaningful basic data types, made after LLVM's
// `FuzzedDataProvider`.
class LibfuzzerReader {
 public:
  LibfuzzerReader(const uint8_t* data, size_t size)
      : data_(data), size_(size) {}

  bool read_bool() { return static_cast<bool>(read_byte() & 1); }

  template <typename T>
  T read_int() {
    return read_int(std::numeric_limits<T>::min(),
                    std::numeric_limits<T>::max());
  }

  template <typename T>
  T read_int(T min, T max) {
    assert(min <= max);

    uint64_t range = static_cast<uint64_t>(max) - min;
    uint64_t value = 0;

    for (size_t i = 0; i < 8 * sizeof(T) && (range >> i) > 0; i += 8) {
      value |= (static_cast<uint64_t>(read_byte()) << i);
    }

    if (range != std::numeric_limits<uint64_t>::max()) {
      value %= (range + 1);
    }

    return static_cast<T>(min + value);
  }

  template <typename T>
  T read_float(T min, T max) {
    assert(min <= max);
    assert(
        (min >= 0 || max <= 0 || min + std::numeric_limits<T>::max() >= max) &&
        "Range cannot be represented with a floating point type!");

    using IntType = typename std::conditional<sizeof(T) <= sizeof(uint32_t),
                                              uint32_t, uint64_t>::type;

    T frac = static_cast<T>(read_int<IntType>()) /
             static_cast<T>(std::numeric_limits<IntType>::max());

    return min + (max - min) * frac;
  }

  uint8_t read_byte() { return offset_ < size_ ? data_[offset_++] : 0; }

  size_t offset() const { return offset_; }

 private:
  const uint8_t* data_;
  size_t size_;
  size_t offset_ = 0;
};

// A class that converts a sequence of meaningful basic type data to a byte
// sequence, compatible with the `LibfuzzerReader`.
class LibfuzzerWriter {
 public:
  LibfuzzerWriter(uint8_t* data, size_t max_size)
      : data_(data), max_size_(max_size) {}

  void write_bool(bool value) { write_byte(value ? 1 : 0); }

  template <typename T>
  void write_int(T value) {
    write_int(value, std::numeric_limits<T>::min(),
              std::numeric_limits<T>::max());
  }

  template <typename T>
  void write_int(T value, T min, T max) {
    assert(min <= value);
    assert(value <= max);
    uint64_t range = static_cast<uint64_t>(max) - min;
    uint64_t write_value = static_cast<uint64_t>(value) - min;
    value -= min;
    for (; range > 0; range >>= 8) {
      write_byte(static_cast<uint8_t>(write_value & 0xff));
      write_value >>= 8;
    }
  }

  template <typename T>
  void write_float(T value, T min, T max) {
    assert(min <= value);
    assert(value <= max);
    assert(
        (min >= 0 || max <= 0 || min + std::numeric_limits<T>::max() >= max) &&
        "Range cannot be represented with a floating point type!");

    using IntType = typename std::conditional<sizeof(T) <= sizeof(uint32_t),
                                              uint32_t, uint64_t>::type;

    if (min == max) {
      write_int<IntType>(0);
      return;
    }

    IntType converted_value = static_cast<IntType>(
        (value - min) / (max - min) *
        static_cast<T>(std::numeric_limits<IntType>::max()));

    write_int(converted_value);
  }

  void write_byte(uint8_t byte) {
    if (size_ < max_size_) {
      data_[size_++] = byte;
    }
  }

  size_t size() const { return size_; }

 private:
  uint8_t* data_;
  size_t max_size_;
  size_t size_ = 0;
};

}  // namespace fuzzer

#endif  // INCLUDE_LIBFUZZER_UTILS_H
