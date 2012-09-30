##' Split data frame, apply columns to arguments of a function, and return results in a data frame.
##'
##' For each subset of a data frame, apply a multiple-argument
##' function, then combine the results into a data frame.
##'
##' @template dm-
##' @template -d
##' @export
##' @examples
##' dmdply(baseball, .(year), function(year) length(year))
##' #or equivalently
##' dmdply(baseball, .(year), fun(length(year)))
##'
##' rbi <- dmdply(baseball, .(year),
##'      summariser(mean_rbi = mean(rbi, na.rm=TRUE)))
##' with(rbi, plot(year, mean_rbi, type="l"))
##'
##' base2 <- dmdply(baseball, .(id),
##'     mutator(career_year=year - min(year) + 1))
dmdply <- function(.data, .variables, .fun=NULL, ..., .progress="none", .drop=TRUE, .parallel=FALSE) {
  stop("not written")
}

##' Split data frame, apply columns to arguments of a function, and return results in a list.
##'
##' For each subset of a data frame, apply the columns of the data
##' frame to a multiple-argument function, then combine the results
##' into a data frame.
##'
##' @template dm-
##' @template -l
dmlply <- function(.data, .variables, fun=NULL, ..., .progress="none") {
  stop("not written")
}

##' Split data frame, apply columns to arguments of a function, and return results in an array.
##'
##' For each subset of a data frame, apply the columns to the
##' arguments of a multiple-argument function, then combione the
##' results into a data frame.
##'
##' @template dm-
##' @template -a
##' @export
##' @examples
##' daply(baseball[, c(2, 6:9)], .(year), argwise(mean))
##' daply(baseball[, c(6:9)], .(baseball$year), argwise(mean))
##' daply(baseball, .(year), argwise(mean, .args=names(df[6:9])))
dmaply <- function(.data, .variables, .fun=NULL, ..., .progress="none", .drop_i = TRUE, .drop_o = TRUE, .parallel=TRUE) {
  stop("not written")
}
