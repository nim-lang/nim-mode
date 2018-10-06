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
