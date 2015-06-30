type
  Node = ref NodeObj
  NodeObj {.acyclic, final.} = object
  left, right: Node
  data: string

type
  Direction = enum
  north, east, south, west

type
  Person = tuple   # type representing a person
  name: string   # a person consists of a name
  age: natural   # and an age

type
  Unit = ref object of Thing
  x: int
