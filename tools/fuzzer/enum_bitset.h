/*
 * Copyright 2020 Google LLC
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

#ifndef INCLUDE_ENUM_BITSET_H
#define INCLUDE_ENUM_BITSET_H

#include <bitset>
#include <initializer_list>
#include <type_traits>
#include <typeindex>  // forward references `std::hash`

namespace fuzzer {

/*
 * A convenient and typesafe wrapper around `std::bitset` intended to be used
 * with enums.
 *
 * Enums that can be used with `EnumBitset` must:
 * - Have a member value `EnumFirst`, which corresponds to the first enum value
 * and its underlying value is always zero.
 * - Have a member value `EnumLast`, which corresponds to the last enum value
 * and its underlying value must be greater than that of `EnumFirst`.
 * - There should be no discontinuities in the underlying values of the enum
 * member values (e.g. `enum class A { V1 = 0, V2 = 5, V3 = 6 };`).
 * - EnumBitset supports enums with up to 64 member values (the reason for this
 * limitation is because an `std::bitset` can only be constructed in a
 * `constexpr` manner if it can be represented by a 64-bit integer).
 *
 * Here's an example of an enum that can be used with EnumBitset:
 * ```
 * enum class CvQualifier {
 *   EnumFirst,
 *   Const = EnumFirst,
 *   Volatile,
 *   EnumLast = Volatile,
 * };
 * ```
 */
template <typename Enum>
class EnumBitset {
 private:
  static_assert(std::is_enum_v<Enum>,
                "`EnumBitset can be only used with enum types");

  using UnderlyingType = std::underlying_type_t<Enum>;

  static_assert((UnderlyingType)Enum::EnumFirst == 0,
                "Enum must have an EnumFirst field that's set to `0`.");
  static_assert((UnderlyingType)Enum::EnumLast > 0,
                "Enum must have an EnumLast field whose underlying value is "
                "greater than `0`.");
  static_assert(
      (UnderlyingType)Enum::EnumLast < 64,
      "EnumBitset doesn't support enums with more than 64 members, sorry.");

  static constexpr size_t BITSET_SIZE = (size_t)Enum::EnumLast + 1;
  using BitsetType = std::bitset<BITSET_SIZE>;

  friend ::std::hash<fuzzer::EnumBitset<Enum>>;

  constexpr unsigned long long initializer_list_value(
      std::initializer_list<Enum> list) {
    unsigned long long value = 0;
    for (auto e : list) {
      value |= 1ull << (size_t)e;
    }
    return value;
  }

 public:
  constexpr EnumBitset() = default;
  constexpr EnumBitset(std::initializer_list<Enum> list)
      : bitset_(initializer_list_value(list)) {}
  constexpr EnumBitset(Enum value) : bitset_(1ull << (size_t)value) {}

  static constexpr EnumBitset all_set() { return EnumBitset(~0ull); }

  constexpr size_t size() const { return BITSET_SIZE; }

  EnumBitset operator&(const EnumBitset& rhs) const {
    return EnumBitset(bitset_ & rhs.bitset_);
  }

  EnumBitset operator|(const EnumBitset& rhs) const {
    return EnumBitset(bitset_ | rhs.bitset_);
  }

  EnumBitset operator^(const EnumBitset& rhs) const {
    return EnumBitset(bitset_ ^ rhs.bitset_);
  }

  EnumBitset operator&(Enum value) const {
    return EnumBitset(bitset_ & EnumBitset(value));
  }

  EnumBitset operator|(Enum value) const {
    return EnumBitset(bitset_ | EnumBitset(value));
  }

  EnumBitset operator^(Enum value) const {
    return EnumBitset(bitset_ ^ EnumBitset(value));
  }

  constexpr EnumBitset operator~() const { return EnumBitset(~bitset_); }

  explicit operator bool() const { return any(); }

  EnumBitset& operator&=(const EnumBitset& rhs) {
    bitset_ &= rhs.bitset_;
    return *this;
  }

  EnumBitset& operator|=(const EnumBitset& rhs) {
    bitset_ |= rhs.bitset_;
    return *this;
  }

  EnumBitset& operator^=(const EnumBitset& rhs) {
    bitset_ ^= rhs.bitset_;
    return *this;
  }

  EnumBitset& operator&=(Enum value) {
    bitset_ &= EnumBitset(value);
    return *this;
  }

  EnumBitset& operator|=(Enum value) {
    bitset_ |= EnumBitset(value);
    return *this;
  }

  EnumBitset& operator^=(Enum value) {
    bitset_ ^= EnumBitset(value);
    return *this;
  }

  bool operator==(const EnumBitset& rhs) const {
    return bitset_ == rhs.bitset_;
  }

  bool operator!=(const EnumBitset& rhs) const {
    return bitset_ != rhs.bitset_;
  }

  bool operator==(Enum value) const { return *this == EnumBitset(value); }

  bool operator!=(Enum value) const { return *this != EnumBitset(value); }

  bool operator[](Enum value) const { return bitset_[(size_t)value]; }
  typename BitsetType::reference operator[](Enum value) {
    return bitset_[(size_t)value];
  }

  bool operator[](size_t idx) const { return bitset_[idx]; }
  typename BitsetType::reference operator[](size_t idx) { return bitset_[idx]; }

  bool all() const { return bitset_.all(); }
  bool any() const { return bitset_.any(); }
  bool none() const { return bitset_.none(); }

  size_t count() const { return bitset_.count(); }

 private:
  explicit constexpr EnumBitset(BitsetType bitset) : bitset_(bitset) {}

  BitsetType bitset_;
};

}  // namespace fuzzer

namespace std {

template <typename Enum>
struct hash<fuzzer::EnumBitset<Enum>> {
  size_t operator()(const fuzzer::EnumBitset<Enum>& bitset) const {
    using BitsetType = typename fuzzer::EnumBitset<Enum>::BitsetType;
    return std::hash<BitsetType>{}(bitset.bitset_);
  }
};

}  // namespace std

#endif  // INCLUDE_ENUM_BITSET_H
