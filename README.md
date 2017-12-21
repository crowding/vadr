vadr
======

The Death Star is blowing up. `vadr` is being split into a few
smaller, more focused packages that will actually make it to CRAN.

```
* memo  * fexpr  * interp
|       |        |
+---+---+--------+
    |
    * macro
    |
    +-- * chain
    .
    .
    .
    +-- * match
    |   |
    |   * syntax
    |
    +-- * iter
```

### (vadr's pieces)

* [`memo`](https://github.com/crowding/memo): An in-memory cache (used for macro expansions)
* [`fexpr`](https://github.com/crowding/fexpr): to !ols to manipulate R promises, unevaluated arguments, missing arguments and `...` lists.
* `interp`: Ruby-style string interpolation.
* `macro`: Common Lisp style macros and quasiquotation operators for R.
* `chain`: Pipeline-style computations, faster and more expressive than magrittr.

### (future developments):

* `match`: Python-style destructuring binds and ML_style pattern matching
* `iter`: Python-style list comprehensions and iterators
* `syntax`: Scheme-style hygeinic(ish) macros

Original documentation follows...

vadr
=====

> _R has been seduced by the dark S of the Force. It is more PHP now
> than Lisp. Its mind is twisted and evil._

> __But there is good in R, I can feel it. I can save it. I have to try.__

R is a curious language. At its core is a Lisp interpreter with
first-class environments and lazy evaluation implemented in terms of
underlying [fexpr]s. It's a language whose core was made flexible
enough to reimplement a weird old language like S-PLUS on top of.

[fexpr]: http://axisofeval.blogspot.com/2012/03/why-fexprs-part-n-or-lambda-only.html

Oddly, all the good bits of R seem to have been buried under
an implementation of weird old S-PLUS.

I like the core language trapped underleath there. It's a bit like
what John Shutt was talking about in his thesis on
[Kernel][kernel]. I'd like to elevate the core above the S facade.

[kernel]: http://web.cs.wpi.edu/~jshutt/kernel.html

Luckily, R is one of the most syntactically malleable languages out there,
if you look at it right.

>_Look at your favorite language. Then at R. Then back to your
>favorite language. Then back to R, Sadly, R isn't your favorite
>language. But it could smell like your favorite language. Look
>down. Now back to R. Where are you? You're writing code in the
>language your language could smell like. Anything is possible.
>R's on a horse._

This package implements workalikes for the author's (and perhaps your)
favorite features from other languages, making R programs shorter and
more expressive. Here are some samples of what you can do:

## Destructuring bind

`bind[]` assigns to several variables at once, by unpacking a list. Say
you have some data coming in with an awkward, messy format, and you
want to extract and reorganize some of the data.

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
[Dagwood Sandwich Code](http://c2.com/cgi/wiki?ThickBreadSmell).

*Example:* Let's compute the perimeter of the 137-gon inscribed in the
unit circle.

If you are comfortable with an array-oriented language (such as R),
you might see this task and think: "Ok, so get the (x,y) coordinates
of the vertices, then difference them to get edge lengths, then add
lengths up for the perimiter."

You could write it serial assignment style, until you run out of patience
for naming things:

```r
n <- 137
radians <- seq(0, 2*pi, length=n+1)
coords <- cbind(sin(radians), cos(radians))
differences <- apply(coords, 2, diff)
segment.lengths <- sqrt(rowSums(differences^2))
perimeter <- sum(segment.lengths)
```

Or you could write inscrutable Dagwood sandwich style, where, for
example, the `2` and the `^2` wind up an enormous distance from the functions
(`apply` and `rowSums`) they are argument or subsequent to:

```r
n <- 137
sum(sqrt(rowSums(apply(sapply(c("sin", "cos"), do.call,
                              list(seq(0, 2*pi, length=137))),
                       2, diff)^2)))
```

This package provides an alternative for this kind of code,
`chain`. Here's `chain` style. It's a bit like a Unix pipeline, and a
bit more like the `->` macro in Clojure. It is compact and reads
well; things start at the beginning and you read along to
the end, no jumping around, the 2 is right next to `apply` where it
belongs and it's not junked up with a bunch of temporary names.

```r
n <- 137
perimeter <- chain(n,
                   seq(length=.+1, 0, 2*pi),
                   cbind(sin(.), cos(.)),
                   apply(2, diff),
                   .^2, rowSums, sqrt, sum)
```

You can narrate this left to right. "Start with your number of
sides. Sample that many times (plus one, oh fenceposts) over [0, 2*pi].
Sine and cosine of that gives you coordinates. Take differences and apply
Pythagoras, squaring, summing and rooting to get the length of each side.
Add it all up and you have your perimiter."

## Easy string substitution

The "%#%" operator splices data into strings much like Python's "%" or Ruby's "#".

```r
".(x), .(y)!" %#% c(x="Hello", y="World!")
```

But this being R, we can do it on collections of strings too. Here's a "[99 bottles][bottles]" implementation:

[bottles]: http://www.99-bottles-of-beer.net/

```r
bottles <- interply(
  ".(ifelse(n%%100>0, n%%100, 'no more')) bottle.('s'[n%%100!=1]) of beer")
initcap <- function(x) {substr(x, 1, 1) <- toupper(substr(x, 1, 1)); x}
verse <- interply(
  paste0(".(initcap(bottles(n=n))) on the wall, .(bottles(n=n)).\n",
         ".(c('Go to the store and buy some more,',",
         "    'Take one down and pass it around,')[(n%%100!=0)+1])",
         " .(initcap(bottles(n=n-1))) on the wall."))
cat(verse(n=99:0), sep="\n\n")
```

## Partial application (currying)

If you ever want to provide default arguments to a function before
handing it off somewhere else, or other such tricks, this package
provide both "leftward" and "rightward" partial application functions,
as well as `%()%`, a full-apply which can be less tricksy than
`do.call()`.

```
printReport <- cat %<<<% "Message: " %<<% c(sep="\n", "-----\n")
printReport %()% c("message one", "message two", "message three")
```

These partial application utilities are fully integrated with good
handling for dot-dot-dot lists mentioned in the next section.

## Dot-Dot-Dot lists and missing values

Variadic arguments (`...`) and missing values are two of the trickiest
spots of R's semantics, and there are very few tools to work with them
-- besides `missing` there's `substitute` and `do.call`, both of which
are hairy and mostly serve other purposes. Mostly people treat `...`
as an opaque block to pass along to another function. This package
contains a number of functions that let you work explicitly with `...`
lists, concatenating and subsetting them, while still allowing R's
lazy-evaluation semantics to do the right thing. So a function using
`dots` can decide whether and when to evaluate each of its unnamed
arguments:

```r
inSomeOrder <- function(...) invisible(list %()% sample(dots(...)))
inSomeOrder(print("Boing!"), print("Boom"), print("Tschack!"), print("Ping"),
            print("Zong"), print("Pssh"))
# [1] "Boing!"
# [1] "Zong"
# [1] "Ping"
# [1] "Boom"
# [1] "Pssh"
# [1] "Tschack!"
```

For a more pointed example, consider `switch`. Switch takes its first
argument and uses it to decide which if its subsequent arguments to
evaluate.

Consider trying to implement an R function that has the behavior of
`switch` properly (not as a C function, and not inspecting the
stack using `match.call()` or `parent.frame()` which are evil.) This
is doable in pure R but wacky and slow -- the only way I can see to selectively evaluate one named argument is to build a function that takes that argument:

```r
switch2 <- function(expr, ...) {
  n <- names(substitute(list(...)))[-1]
  if (!is.null(n))
      arglist <- as.pairlist(structure(
          rep(list(quote(expr=)), length(n)),
          names=n))
  else
      (arglist <- as.pairlist(alist(...=)))

  if (is.numeric(expr))
      body <- as.name(paste0("..", expr))
  else
      body <- as.name(expr)
  f <- eval(substitute(`function`(arglist, body),
                         list(arglist=arglist, body=body)))
  f(...)
}
```

But with a direct interface to manipulate dotlists, `switch` is easy:

```r
switch3 <- function(expr, ...) {
  dots(...)[[expr]]
}
```

You may also use `dots_unpack()` to inspect the contents of
as-yet-unevaluated dots objects, exposing R's promise mechanism:

```r
x <- 1
y <- 2
d <- dots(a=x, b=y, c=x+y)
unpack(d)
#   name         envir  expr value
# a    a <environment>     x  NULL
# b    b <environment>     y  NULL
# c    c <environment> x + y  NULL
# > y <- 3
(function(b, ...) b) %()% d #force the "b" slot to evaluate
# [1] 3
unpack(d)
#   name         envir  expr value
# a    a <environment>     x  NULL
# b    b          NULL     y     3
# c    c <environment> x + y  NULL
c %()% d
# a b c
# 1 3 4
> unpack(d)
#   name envir  expr value
# a    a  NULL     x     1
# b    b  NULL     y     3
# c    c  NULL x + y     4
```

## Quasiquotation

`qq` implements quasiquotation, which is a way to build expressions and code out of data. `qq` is more capable than `substitute` or `bquote` and faster than the latter.  Think 'string substitution' as above, but for syntactically correct code. See the `qq` vignette for more details.

## Macros

Many of the facilities in `vadr` are implemented in terms of macros. Macros work similarly to the computing-on-the-language facilities you may be familiar with, but much of their work can be memoized so they can be faster. Vignette to follow.
