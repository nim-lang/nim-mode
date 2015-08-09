# Test uncompleted condition (multiple line condition's indentation)

# This test set `nim-uncompleted-condition-indent' to 4, which is default.

var looongCondition = true

# if statement
if "Even if string's :" == "is exist" and
"The : should not affect" == "indent result":
echo "foo"

# when statement
when "Even if string's :" == "is exist" and
"The : should not affect" == "indent result":
echo "foo"

# while statement
while "Even if string's :" == "is exist" and
"The : should not affect" == "indent result":
echo "foo"

# elif statement
case true
elif "Even if string's :" == "is exist" and
"The : should not affect" == "indent result":
echo "foo"

# same test with proc
proc testIfStmt() =
if "Even if string's :" == "is exist" and
"The : should not affect" == "indent result":
echo "foo"

proc testWhenStmt() =
when "Even if string's :" == "is exist" and
"The : should not affect" == "indent result":
echo "foo"

proc testWhileStmt() =
while "Even if string's :" == "is exist" and
"The : should not affect" == "indent result":
echo "foo"

proc testElifStmt() =
case true
elif "Even if string's :" == "is exist" and
"The : should not affect" == "indent result":
echo "foo"

# 3 condition lines
proc 3condtionLines() =
if looongCondition and looongCondition and
looongCondition and looongCondition or
looongCondition and looongCondition:
echo "foo"
