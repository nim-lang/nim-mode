# Inside args' ";" treated as ","
proc hello(a: string, b: string;
           c: string) =
  echo a b c
