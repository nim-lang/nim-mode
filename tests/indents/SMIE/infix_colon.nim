# smie only care about `smie-closer-alist`s tokens. So, in following
# situation, indent of line 9 (Check this...) referred ":" below
# "object" before I fixed the indent.
type
  A = object
    str: string


echo "foo" "bar"
echo "Check this line's indent"
