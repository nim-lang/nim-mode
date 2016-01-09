In this directory, all Nim's codes are unformatted and then
tested by `indent-region` function.

For example, if you have following code:


```nim
if true:
  echo "hello"
else true:
  echo "world"
```

is converted to


```nim
if true:
echo "hello"
else true:
echo "world"
```

So, you just need to place `expected` indentation codes in this
directory here.

Note that, the unformat function will erase even if it's string's
spaces, in such case test that in ./smie/raw directory.
