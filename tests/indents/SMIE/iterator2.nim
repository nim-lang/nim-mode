proc mycount(a, b: int): iterator (): int =
  result = iterator (): int =
             var x = a
             while x <= b:
               yield x
               inc x
               break
