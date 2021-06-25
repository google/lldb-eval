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

#include "tools/fuzzer/libfuzzer_utils.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

using namespace fuzzer;
using namespace testing;

void attach_byte_writer(LibfuzzerWriter& writer, ByteWriter& byte_writer) {
  writer.set_callback([&](uint8_t byte) { byte_writer.write_byte(byte); });
}

TEST(LibfuzzerUtils, Bool) {
  uint8_t data[2];
  LibfuzzerReader reader(data, 2);
  LibfuzzerWriter writer;
  ByteWriter byte_writer(data, 2);
  attach_byte_writer(writer, byte_writer);

  writer.write_bool(false);
  writer.write_bool(true);

  EXPECT_EQ(reader.read_bool(), false);
  EXPECT_EQ(reader.read_bool(), true);
}

TEST(LibfuzzerUtils, Int) {
  uint8_t data[256];
  LibfuzzerReader reader(data, 256);
  LibfuzzerWriter writer;
  ByteWriter byte_writer(data, 256);
  attach_byte_writer(writer, byte_writer);

  writer.write_int<int>(12345);
  EXPECT_EQ(reader.read_int<int>(), 12345);

  writer.write_int<int8_t>(0);
  writer.write_int<int8_t>(-1);
  writer.write_int<int8_t>(-128);
  writer.write_int<int8_t>(127);
  EXPECT_EQ(reader.read_int<int8_t>(), 0);
  EXPECT_EQ(reader.read_int<int8_t>(), -1);
  EXPECT_EQ(reader.read_int<int8_t>(), -128);
  EXPECT_EQ(reader.read_int<int8_t>(), 127);

  writer.write_int<uint8_t>(0);
  writer.write_int<uint8_t>(255);
  EXPECT_EQ(reader.read_int<uint8_t>(), 0);
  EXPECT_EQ(reader.read_int<uint8_t>(), 255);

  writer.write_int<int64_t>(0);
  writer.write_int<int64_t>(-1);
  writer.write_int<int64_t>(-9223372036854775808ULL);  // -2^63
  writer.write_int<int64_t>(9223372036854775807LL);    // 2^63 - 1
  EXPECT_EQ(reader.read_int<int64_t>(), 0);
  EXPECT_EQ(reader.read_int<int64_t>(), -1);
  EXPECT_EQ(reader.read_int<int64_t>(), -9223372036854775808ULL);
  EXPECT_EQ(reader.read_int<int64_t>(), 9223372036854775807LL);

  writer.write_int<int>(100, -500, 500);
  EXPECT_EQ(reader.read_int<int>(-500, 500), 100);

  writer.write_int<int64_t>(123456789123LL, 123456789000LL, 123456789123LL);
  EXPECT_EQ(reader.read_int<int64_t>(123456789000LL, 123456789123LL),
            123456789123LL);
}

TEST(LibfuzzerUtils, Float) {
  uint8_t data[256];
  LibfuzzerReader reader(data, 256);
  LibfuzzerWriter writer;
  ByteWriter byte_writer(data, 256);
  attach_byte_writer(writer, byte_writer);

  const float feps = std::numeric_limits<float>::epsilon();
  const double deps = std::numeric_limits<double>::epsilon();

  writer.write_float<float>(1.f, 0.f, 3.f);
  writer.write_float<float>(-1000.1f, -2000.f, 5000.f);
  EXPECT_THAT(reader.read_float<float>(0.f, 3.f), FloatNear(1.f, 3 * feps));
  EXPECT_THAT(reader.read_float<float>(-2000.f, 5000.f),
              FloatNear(-1000.1f, 7000 * feps));

  writer.write_float<double>(3.14, 2.71, 3.14159);
  writer.write_float<double>(-500.0, -650.0, -400.0);
  EXPECT_THAT(reader.read_float<double>(2.71, 3.14159), DoubleNear(3.14, deps));
  EXPECT_THAT(reader.read_float<double>(-650.0, -400.0),
              DoubleNear(-500.0, 250 * deps));
}

TEST(LibfuzzerUtils, Sizes) {
  uint8_t data[256];
  LibfuzzerReader reader(data, 256);
  LibfuzzerWriter writer;
  ByteWriter byte_writer(data, 256);
  attach_byte_writer(writer, byte_writer);

  writer.write_bool(false);
  (void)reader.read_bool();
  EXPECT_EQ(byte_writer.size(), 1);
  EXPECT_EQ(reader.offset(), 1);

  writer.write_byte(123);
  (void)reader.read_byte();
  EXPECT_EQ(byte_writer.size(), 2);
  EXPECT_EQ(reader.offset(), 2);

  writer.write_int<char>(44);
  (void)reader.read_int<char>();
  EXPECT_EQ(byte_writer.size(), 3);
  EXPECT_EQ(reader.offset(), 3);

  writer.write_int<short>(456);
  (void)reader.read_int<short>();
  EXPECT_EQ(byte_writer.size(), 5);
  EXPECT_EQ(reader.offset(), 5);

  writer.write_int<int32_t>(1000);
  (void)reader.read_int<int32_t>();
  EXPECT_EQ(byte_writer.size(), 9);
  EXPECT_EQ(reader.offset(), 9);

  writer.write_int<long long>(1LL);
  (void)reader.read_int<long long>();
  EXPECT_EQ(byte_writer.size(), 17);
  EXPECT_EQ(reader.offset(), 17);

  // Bounds difference is 255.
  writer.write_int<int>(1337, 1300, 1555);
  (void)reader.read_int<int>(1300, 1555);
  EXPECT_EQ(byte_writer.size(), 18);
  EXPECT_EQ(reader.offset(), 18);

  // Bounds difference is 256.
  writer.write_int<uint64_t>(100200300400500600LL, 100200300400500600LL,
                             100200300400500856LL);
  (void)reader.read_int<uint64_t>(100200300400500600LL, 100200300400500856LL);
  EXPECT_EQ(byte_writer.size(), 20);
  EXPECT_EQ(reader.offset(), 20);

  writer.write_float<float>(1.f, 0.f, 2.f);
  (void)reader.read_float<float>(0.f, 2.f);
  EXPECT_EQ(byte_writer.size(), 24);
  EXPECT_EQ(reader.offset(), 24);

  writer.write_float<double>(1.0, 0.0, 2.0);
  (void)reader.read_float<double>(0.0, 2.0);
  EXPECT_EQ(byte_writer.size(), 32);
  EXPECT_EQ(reader.offset(), 32);
}

TEST(LibfuzzerUtils, Overflow) {
  uint8_t data[1];
  LibfuzzerReader reader(data, 1);
  LibfuzzerWriter writer;
  ByteWriter byte_writer(data, 1);
  attach_byte_writer(writer, byte_writer);

  writer.write_int<uint16_t>(257);
  EXPECT_EQ(reader.read_int<uint16_t>(), 1);

  writer.write_byte(12);
  writer.write_bool(true);
  writer.write_float(3.14f, 0.f, 10.f);

  EXPECT_EQ(reader.read_byte(), 0);
  EXPECT_EQ(reader.read_bool(), false);
  EXPECT_EQ(reader.read_float(0.f, 10.f), 0.f);

  EXPECT_EQ(byte_writer.size(), 1);
  EXPECT_EQ(reader.offset(), 1);
}
