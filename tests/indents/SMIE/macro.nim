macro `[]`*[A, B](t: var Table[A, B], key: A): var B =
  let opt = getImpl(t, key)
  foo

proc testMacro =
  dumpTree:
    foo:
      x = "bar"
