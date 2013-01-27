ptools
======

Look at your favorite language. Then at R. Then back to your 
favorite language. Then back to R, Sadly, R isn't your favorite 
language. But it could smell like your favorite language. Look 
down. Now back to R. Where are you? You're writing code in the 
language your language could smell like. Anything is possible. 
R's on a horse. 

This package implements workalikes for the author's (and perhaps your)
favorite features from other languages, making R programs shorter and
more expressive. Here are some samples of what you can do:

## Destructuring bind

`bind[]` assigns to several variables at once, by unpacking a list. Say
you have some data coming in with an awkward, messy format, and you
want to pull a bunch of values from it.

```r
record <- list("Marilyn", "Monroe", dob=list("June", 1, 1926),
               profession="film star", "born Norma Jean Baker",
               fbi_file_no="FFIJ8SN_65",
               "1947 California Artichoke Queen",
               list("August", 5, 1962))
```

You could do the ordinary way:

```r
first <- record[[1]]
last <- record[[2]]
bday <- record$dob[[2]]
bmonth <- record$dob[[1]]
byear <- record$dob[[3]]
dday <- record[[length(record)]][[2]]
dmonth <- record[[length(record)]][[1]]
dyear <- record[[length(record)]][[3]]
record[["fbi_file_no"]] <- NULL
notes <- #....uhh, everything else, somehow?
```

My eyes glaze over. Or you could use `bind[]`:

```
bind[first, last, dob=bind[bmonth, bday, byear],
     fbi_file_no=, ...=notes, bind[dmonth, dday, dyear]] <- record
```

## Chains

You ever take some data and pass it through a function, then pass the
result theough another function, then pass that through another
function, in a series of steps? I do that all the time.

You basically have two options for how to write such code: assign
every intermediate result to a var, probably reusing the same variable
name, which I hate because I don't want to give a name to data until
it actually _is_ what it's name suggests; or do it all at once in a
deeply nested function call, which gets you
[Dagwood Sandwich Code][http://c2.com/cgi/wiki?ThickBreadSmell].

*Example:* Let's compute the perimeter of the 137-gon inscribed in the
unit circle, by computing the coordinates of each point and from that
distance along each segment.

You could do it serial assignment style, until you run out of patience
for naming things:

```r
n <- 137
radians <- seq(0, 2*pi, length=137)
coords <- cbind(sin(radians), cos(radians))
differences <- apply(coords, 2, diff)
segment.lengths <- sqrt(rowSums(differences^2))
perimeter <- sum(segment.lengths)
```

Or you would do inscrutable Dagwood sandwich style, where, for
example, the `2` winds up an enormous distance from the function
(`apply`) it is an argument to:

```r
sum(sqrt(rowSums(apply(sapply(c("sin", "cos"), do.call,
                              list(seq(0, 2*pi, length=137))),
                       2, diff)^2)))
```

This package provides an alternative for this kind of code,
`chain`. Here's `chain` style. It's a bit like a Unix pipeline, and a
bit more like the `->` macro in Clojure, and it's compact and easy to
read all at once. Things happen at the beginning and you read along to
the end, no jumping around, the 2 is right next to 'apply' where it
belongs and it's not junked up with a bunch of temporary names.

```r
perimeter <- chain(137, seq(0, 2*pi, length=.), cbind(sin(.), cos(.)),
                   apply(2, diff), .^2, rowSums, sqrt, sum)
```

## Partial application (currying)

If you ever want to provide default arguments to a function before
handing it off somewhere else, or other such tricks, this package
provide both "leftward" and "rightward" partial application functions,
as well as `%()%`. a full-apply which can be less tricksy than
`do.call()`.

```
printReport <- "Message: " %>>% cat %<<% c(sep="\n", "-----\n")
printReport %()% c("message one", "message two", "message three")
```

These partial application utilities are fully integrated with good
handling for dot-dot-dot lists mentioned in the next sections

## Dot-Dot-Dot lists and missing values

Variadic arguments (`...`) and missing values are two of the trickiest
spots of R's semantics, and there are very few tools to work with them
-- besides `missing` there's `substitute` and `do.call`, both of which
are hairy and mostly serve other purposes. Mostly people treat `...`
as an opaque block to pass along to another function. This package
contains a number of functions that let you work explicitly with `...`
lists, concatenating and subsetting them, while still allowing R's
lazy-evaluation semantics to do the right thing.

```r
> scrambleMiddle <- function(...) {
   d <- dots(...)
   d[seq(2, len=length(d)-2)] <- sample(d[seq(2, len=length(d)-2)])
   (cat %<<% "\n") %()% d
 }
> scrambleMiddle("alpha", "bravo", "charlie", "delta", "echo", "foxtrot")
alpha delta echo bravo charlie foxtrot
```

You may also use `dots_unpack()` to inspect the contents of
as-yet-unevaluated dots objects, exposing R's promise mechanism:

```r
> x <- 1
> y <- 2
> d <- dots(a=x, b=y, c=x+y)
> unpack(d)
  name         envir  expr value
a    a <environment>     x  NULL
b    b <environment>     y  NULL
c    c <environment> x + y  NULL
> y <- 3
> (function(b, ...) b) %()% d #force the "b" slot to evaluate
[1] 3
d> unpack(d)
  name         envir  expr value
a    a <environment>     x  NULL
b    b          NULL     y     3
c    c <environment> x + y  NULL
> c %()% d
a b c
1 3 4
> unpack(d)
  name envir  expr value
a    a  NULL     x     1
b    b  NULL     y     3
c    c  NULL x + y     4
```
