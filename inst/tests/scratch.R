e <- new.env(hash=TRUE)
evalq(x<-5, e)evalq(exists("x", inherits=FALSE), e)
evalq(rm("x"), e)
evalq(exists("x", inherits=FALSE), e)

exists("foo", inherits=FALSE)
foo <- 5
rm("foo")


#regexec which is the only regex variant that actually extracts the
#match, and it doesn't support perl, wtf!

#match balanced R-ish expressions.
sexp.re <- "(?x)
 ([a-z]+)
"
grep(sexp.re, "hello (what (a maroon) this is) a thing", value=TRUE, perl=TRUE)

"      \\(                   # Actual open paren symbol
        (                  # A group of either
           [^()\"'%`]+     # Not parens or quotes
         |
           \"[^\"]\"       # Quote
         |                 # Or
           (?R)            # Recurse.
        )*                 # Match as many times as needed.
      \\)                  # Actual close paren
"

