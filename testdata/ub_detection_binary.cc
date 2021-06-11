#include <limits>

enum class ScopedEnum { kZero, kOne };

int main() {
  int i = 1;
  int* inp = nullptr;
  std::nullptr_t null_ptr = nullptr;
  std::nullptr_t& null_ptr_ref = null_ptr;
  std::nullptr_t* null_ptr_addr = &null_ptr;

  struct S {
    std::nullptr_t null_field = nullptr;
  } s;

  float finf = std::numeric_limits<float>::infinity();
  float fnan = std::numeric_limits<float>::quiet_NaN();
  float fsnan = std::numeric_limits<float>::signaling_NaN();
  float fmax = std::numeric_limits<float>::max();
  float fdenorm = std::numeric_limits<float>::denorm_min();

  double dinf = std::numeric_limits<double>::infinity();
  double dnan = std::numeric_limits<double>::quiet_NaN();
  double dsnan = std::numeric_limits<double>::signaling_NaN();
  double dmax = std::numeric_limits<double>::max();
  double ddenorm = std::numeric_limits<double>::denorm_min();

  auto scoped_enum = ScopedEnum::kZero;

  // BREAK HERE

  return 0;
}
