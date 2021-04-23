# Bugs in LLDB

## Comparing `lldb::SBType` objects representing the same type doesn't always work

```cpp
>>> v = lldb.frame.FindVariable('x')
>>> v.GetType().GetName()
'int'
>>> lldb.target.GetBasicType(lldb.eBasicTypeInt).GetName()
'int'
>>> v.GetType() == lldb.target.GetBasicType(lldb.eBasicTypeInt)
False
```

Discussion on #lldb channel -- <https://discord.com/channels/636084430946959380/636732809708306432/800681526198272010>

Workaround in `lldb-eval` -- <https://github.com/google/lldb-eval/blob/5add7abc740ceeb43db4ab7c644248dbeb2908c1/lldb-eval/value.cc#L163>

## Inconsistent global variable names for Linux and Windows

```c++
# Definitions
int array2d[10][10];
namespace abc { int array2d[10][10]; }

# Windows
>>> lldb.target.FindFirstGlobalVariable("array2d").GetName()
'int (*array2d)[10]'
>>> lldb.target.FindFirstGlobalVariable("abc::array2d").GetName()
'int (*abc::array2d)[10]'

# Linux
>>> lldb.target.FindFirstGlobalVariable("array2d").GetName()
'::array2d'
>>> lldb.target.FindFirstGlobalVariable("abc::array2d").GetName()
'abc::array2d'
```

Discussion on #lldb channel -- <https://discord.com/channels/636084430946959380/636732809708306432/819528095496667156>

## Dereferencing a value canonizes the type

```c++
>>> x_ref = lldb.frame.FindVariable('x_ref')
>>> x_ref.type.name
'myint &'
>>> x_ref.type.GetDereferencedType().name
'myint'
>>> x_ref.Dereference().type.name
'int'
```

Current workaround in `lldb-eval` -- <https://github.com/google/lldb-eval/commit/c914f65702714b90565b5df5450eadd0fd617658>

## `lldb::SBValue::Persist()` doesn't work for "ephemeral" values

```c++
>>> v = lldb.target.CreateValueFromData(...)
>>> vp = v.Persist()
>>> vp.value
'42'
>>> vp.name
'$0'
>>> lldb.frame.EvaluateExpression('$0').value
'42'
>>> lldb.frame.EvaluateExpression('++$0').value
'43'
>>> lldb.frame.EvaluateExpression('++$0').value
'44'
>>> vp.value
'42'
```

Discussion in Phabricator -- <https://reviews.llvm.org/D98370>
