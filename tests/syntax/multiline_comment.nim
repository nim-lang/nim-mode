###[ <- this is treated as single comment
echo "#[]# is multi line comment"
#[
bla bla bla bla bla bla
# inside hash
#[nesting comment]#
##[nesting comment]##
bla bla bla bla bla bla
]#
echo "##[]## is multi line documentation comment"
##[
multi line documentation comment can include
#[nesting]# and #[nesting] (unbalance #[ is ok)

Also nested docummentation comment: ##[ nested doc comment ]##
(, but need to provide both open ##[ and close ]## )
]##

proc testMultiComment =
  ##[ multi line comment test
  aaa #[nesting]# bbb
  ]##
  echo "foo bar"

var foo #[ comment ]#: string = "foo"

# boundary
echo "#["# foo
echo "]#"# bar
echo "##["# foo
echo "]##"# bar
echo "check ->"#" <- double quote
