type
  Person = tuple   # type representing a person
    name: string   # a person consists of a name
    age: int
    weight: int


type
  Person2 = tuple[name: string, age: int]
  afterTuple = string


type
  Person3 = tuple[
    name: string,
    age: int
  ]
  afterTuple2 = string


# tuple with `var`
var building: tuple[street: string, number: int] = (
  "Rue del Percebe",
  13
)

# check return type's tuple
proc tupleTest*[T](): tuple[key:int, val:var T] =
  echo "this line should be indented correctly"
