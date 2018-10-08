let s = "foobar"

var heredoc = """'one' ''two''
'''three''' ""two double quotes""
"""

var endOfQuote = "foo'"
# this line should be comment face
var endOfQuote2 = """foo'"""
# this line should be comment face

var beginningOfQuote = "'foo"
# this line should be comment face
var beginningOfQuote2 = """'foo"""
# this line should be comment face

var enclosedQuote = "'foo'"
# this line should be comment face
var enclosedQuote2 = """'foo'"""
# this line should be comment face

var escapedSingleQuote = "foo\'"
# this line should be comment face
var escapedSingleQuote2 = """foo\'"""
# this line should be comment face

var escapedDoubleQuote = "foo\""
# this line should be comment face
var escapedDoubleQuote2 = """foo\""""
# this line should be comment face

var unbalancedDoubleQuote = """8 double quotes->""""""""
# this line should be comment face
var unbalancedDoubleQuote2 = """""<-5 double quotes 11 double quotes->"""""""""""
# this line should be comment face
var unbalancedDoubleQuote3 = """"<-4 double quotes"""
# this line should be comment face
var unbalancedDoubleQuote4 = """"enclosed double quotes""""
# this line should be comment face

var rawString = r"foo""bar""buzz"
# this line should be comment face
var rawStringIssue210 = r""
# this line should be comment face

#[ #212 raw string literal with end of `\` ]#
var rawStringWithBackslash = r"\"
# this line should be comment face

var rawStringWithBackslash2 = R2D2"\"
# should be fontified even if identifier has number

var rawStringWithBackslash3 = sql"\"
# this line should be comment face

var rawStringWithBackslash4 = """abc\"""
# this line should be comment face

var rawStringWithBackslash5 = r"a\""bc\"
# this line should be comment face

var rawStringWithBackslash6 = sql"""abc\"""
# this line should be comment face
