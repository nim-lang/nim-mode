# highlight $# or $[1-9][0-9] inside string for `format`
import strutils

echo """
$1 + $2 = $#
""".format("1", "2", "3")
