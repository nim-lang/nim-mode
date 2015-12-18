macro test(): stmt =
  result = newStmtList(
    newNimNode(nnkTypeSection).add(
      newNimNode(nnkTypeDef).add(
        ident("A"),
        newEmptyNode(),
        newNimNode(nnkEnumTy).add(
          newEmptyNode(),
          ident("a"),
          ident("b")))))
