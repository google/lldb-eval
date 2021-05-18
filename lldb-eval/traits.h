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

#include <utility>

// Template magic to check whether a class has a specific method.
template <typename T, typename F>
constexpr auto is_valid(F&& f) -> decltype(f(std::declval<T>()), true) {
  return true;
}
template <typename>
constexpr bool is_valid(...) {
  return false;
}
#define HAS_METHOD(T, EXPR) is_valid<T>([](auto&& obj) -> decltype(obj.EXPR) {})
