type
  Node = ref NodeObj
  NodeObj {.acyclic, final.} = object
    left, right: Node
    data: string
