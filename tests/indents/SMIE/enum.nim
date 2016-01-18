type
  Direction = enum
    north, east,
    south, west


type
  TokenType = enum
    a = 2,
    b = 4,
    c = 89


type # with pragma
  MyEnum {.pure.} = enum
    valueA, valueB,
    valueC, valueD
