\name{index.data.frame}
\alias{index}
\alias{index.data.frame}
\alias{index.data.frame<-}
\alias{index<-}
\title{index.data.frame}
\usage{
  index.data.frame(df, row = 1:nrow(df), col = NULL,
    value = c())
}
\arguments{
  \item{df}{The data frame to subset}

  \item{row}{Rows to subset by. These may be numeric
  indices, character names, a logical mask, or a 2-d
  logical array}

  \item{col}{The columns to index by. If `row` is a 2-d
  array, this should not be given.}

  \item{value}{Provide a an empty vector of some type to
  specify the type of the output.}
}
\value{
  The specified subset of data.
}
\description{
  Improved subsetting and modification of data frames by
  rows, columns and arrays.
}
\details{
  In particular, this handles the case of assigning to
  several elements of a data frame in parallel.

  This handles a couple of cases that are not handled by
  core data frame operations In particular, this allows you
  to extract a vector of values from two vectors of row and
  column indices (which may be numeric or character).

  Unlike using \code{`[.data.frame`} wiht an n-by-2 array,
  this does not downcast the array to matrix, and tries to
  prezerve the type information.
}
\examples{
#Here is a data frame with row names and mixed data types.
df <- data.frame(  A = c(1,4,2,6,7,3,6)
                 , B = c(3,7,2,7,3,5,4)
                 , C = c(2,7,5,2,7,4,5)
                 , index = c("A","B","A","C","B","B","C")
                 , letter = I(letters[7:13])
                 , lletter = I(list("a",1,"b",3,NULL,5,"d"))
                 , row.names = c( "foo", "bar", "baz", "qux"
                                 , "quux", "quuux", "quuuux")


#select values from column A,B,C for every row
index(df, col=c("A","B","A","C","B","B","C") # -> c(1, 7, 2, 2, 3, 5, 5)

#indexing by a 1x2 array extracts a single element unboxed
index(df, array(c(3,2), dim=c(1,2))) # -> 2

#You can also index by a 1x2 char array by row and column names
index(df, array(c("baz", "B"), dim=c(1,2))) # -> 2

# You can also index by a two-column data frame
index(df, data.frame(a=3, b="B")) # -> 2

# We try to pick an appropriate type logical/numeric/character/list)
index(df, c(4,5), c("A", "letter")) # -> c("6", "k")
index(df, c(4,5,6), c("A", "letter", "lletter")) # -> list("6", "k", 5))

# We can do a scattered assignment in the same way
index(df, c(1,2,3), c("A","B","C")) <- c(100, 1000, 10000)
}
\author{
  Peter Meilstrup
}

