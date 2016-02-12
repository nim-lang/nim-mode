# test highlighting a word as type face after `is` and `distinct`

type
  custom_type = tuple[a: string]
  custom_type2 = distinct custom_type # <- highlight

var it: custom_type = (a: "foo")
if it is custom_type:
  echo "custom_type should be highlighted as type face"
elif it is custom_type2:
  echo "custom_type2 should be highlighted as type face"
