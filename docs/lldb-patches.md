# List of patches for LLDB

This document contains a list of patches for `LLDB` that are required (or recommended) for `lldb-eval`.

* (Required) `[LLDB] Fix how ValueObjectVariable handles DW_AT_const_value when the DWARFExpression holds the data that represents a constant value`

  > In some cases when we have a DW_AT_const_value and the data can be found in the
DWARFExpression then ValueObjectVariable does not handle it properly and we end
up with an extracting data from value failed error.

  Differential revision: <https://reviews.llvm.org/D86311>

  Git commit: [93b255142bb7025f62cf83dd5b7d3b04aab5445b](https://github.com/llvm/llvm-project/commit/93b255142bb7025f62cf83dd5b7d3b04aab5445b)

* `[lldb] Add SBType::IsScopedEnumerationType method`

  > Add a method to check if the type is a scoped enumeration (i.e. "enum class/struct").

  Differential revision: <https://reviews.llvm.org/D93690>

  Git commit: [e17a00fc87bc163cc2438ce10faca51d94b91ab3](https://github.com/llvm/llvm-project/commit/e17a00fc87bc163cc2438ce10faca51d94b91ab3)

* `[lldb] Add SBType::GetEnumerationIntegerType method`

  > Add a method for getting the enumeration underlying type.

  Differential revision: <https://reviews.llvm.org/D93696>

  Git commit: [1432ae57bf6e4022b6f4541c9225674ee6b19c23](https://github.com/llvm/llvm-project/commit/1432ae57bf6e4022b6f4541c9225674ee6b19c23)

* `[lldb] Lookup static const members in FindGlobalVariables`

  > Static const members initialized inside a class definition might not have a corresponding DW_TAG_variable, so they're not indexed by ManualDWARFIndex. Add an additional lookup in FindGlobalVariables. Try looking up the enclosing type (e.g. `foo::bar` for `foo::bar::A`) and then searching for a static const member (A) within this type.

  Differential revision: <https://reviews.llvm.org/D92643>

  Git commit: **NOT SUBMITTED**

* `[lldb] Support unscoped enumeration members in the expression evaluator`

  > Add unscoped enumeration members to the "globals" manual dwarf index. This effectively makes them discoverable as global variables (which they essentially are).

  Differential revision: <https://reviews.llvm.org/D94077>

  Git commit: **NOT SUBMITTED**
