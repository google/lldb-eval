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

#ifndef INCLUDE_RNG_H
#define INCLUDE_RNG_H

#include <random>

namespace fuzzer {

typedef std::mt19937::result_type rand_t;

class BaseRng {
 public:
  virtual ~BaseRng() {}

  using result_type = rand_t;
  static constexpr result_type min() { return std::mt19937::min(); }
  static constexpr result_type max() { return std::mt19937::max(); }
  virtual result_type operator()() = 0;

  class Listener {
   public:
    virtual void on_consume_random(rand_t value) = 0;
  };

  void set_listener(Listener* listener) { listener_ = listener; }

 protected:
  result_type consume(result_type value) {
    if (listener_ != nullptr) {
      listener_->on_consume_random(value);
    }
    return value;
  }

 private:
  Listener* listener_ = nullptr;
};

class Mt19937 : public BaseRng {
 public:
  explicit Mt19937(rand_t seed) : rng_(seed) {}

  BaseRng::result_type operator()() { return consume(rng_()); }

 private:
  std::mt19937 rng_;
};

}  // namespace fuzzer

#endif  // INCLUDE_RNG_H
