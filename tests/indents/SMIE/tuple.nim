type
  Person = tuple   # type representing a person
    name: string   # a person consists of a name
    age: int
    weight: int


type
  Person2 = tuple[name: string, age: int]
  afterTuple = "foo"
