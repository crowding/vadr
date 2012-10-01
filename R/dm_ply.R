splitter_d <- plyr:::splitter_d
list_to_dataframe <- plyr:::list_to_dataframe

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
  pieces <- splitter_d(.data, .variables, drop = .drop)
  res <- dm_ply_worker(.data=.data, .pieces=pieces, .fun=.fun, ..., .progress="none", .drop=.drop, .parallel=.parallel)
  list_to_dataframe(res, attr(.data, "split_labels"))

  #now I still need to re-apply ths split columns.
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
  pieces <- splitter_d(.data, .variables, drop = .drop)
  res <- dm_ply_worker(.data=.data, .pieces=pieces, .fun=.fun, ..., .progress=progress, .drop=drop_o, .parallel=.parallel)
  list_to_array(res, attr(.data, "split_labels"), .drop=.drop_o)
}

##' Split data frame, apply columns to arguments of a function, and return results in a list.
##'
##' For each subset of a data frame, apply the columns of the data
##' frame to a multiple-argument function, then combine the results
##' into a data frame.
##'
##' @template dm-
##' @template -l
##' @export
dmlply <- function(.data, .variables, .fun=NULL, ..., .progress="none", drop=TRUE, .parallel=TRYE) {
  pieces <- splitter_d(.data, .variables, drop = .drop)
  res <- dm_ply_worker(.data=.data, .pieces=pieces, .fun=.fun, ..., .progress="none", .drop, .parallel=.parallel)
}

dm_ply_worker <- function(.data, .pieces, .fun=NULL, ..., .progress="none", .drop, .parallel=TRUE) {
  ##now having the splits, we look at the function...
  if (is.null(.fun)) fun <- mutator();
  if (is.character(.fun) || is.list(.fun)) .fun <- each(.fun)
  if (!is.function(.fun)) stop(".fun is not a function.")

  arg.names <- names(formals(.fun))
  args.used <- intersect(names(.data), names(formals(.fun)))
  if ("..." %in% arg.names) {
    rest.args <- setdiff(names(.data), names(formals(.fun)))
  } else {
    rest.args <- character(0)
  }

  #construct our function call. The variable "index" will change.
  args <- lapply(c(args.used, rest.args), function(col) substitute(.data$column[i], list(column=col)))
  names(args) <- c(args.used, rest.args)
  f <- eval(substitute(function(i) c, list(c=as.call(c(as.name(".fun"), args)))))

  #FIXME this should fall back to llply so that parallel can be used.
  test <- lapply(.pieces$index, f)
}

##' Split data frame, apply columns to arguments of a function, and discard results.
##'
##' For each subset of a data frame, apply its columns to a multiple-argument function.
##'
##' @template dm-
##' @template -_
##' @export
dm_ply <- function(.data, .variables, .fun=NULL, ..., .progress="none", .drop=TRUE, .parallel=FALSE) {
  pieces <- splitter_d(.data, .variables, drop = .drop)
  stop("not written")
  invisible()
}
