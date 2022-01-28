#ifndef INCLUDE_LIBFUZZER_COMMON_H
#define INCLUDE_LIBFUZZER_COMMON_H

#include <cstddef>
#include <cstdint>
#include <string>

#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBTarget.h"
#include "tools/fuzzer/symbol_table.h"

namespace fuzzer {

class LibfuzzerState {
 public:
  LibfuzzerState() = default;
  ~LibfuzzerState() {}

  int init(int* argc, char*** argv);

  size_t custom_mutate(uint8_t* data, size_t size, size_t max_size,
                       unsigned int seed);

  std::string input_to_expr(const uint8_t* data, size_t size);

  lldb::SBFrame& frame() { return frame_; }

  lldb::SBTarget& target() { return target_; }

 private:
  lldb::SBDebugger debugger_;
  lldb::SBFrame frame_;
  lldb::SBTarget target_;
  SymbolTable symtab_;
};

}  // namespace fuzzer

#endif  // INCLUDE_LIBFUZZER_COMMON_H
