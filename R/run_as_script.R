##' run_as_command Interpret command line arguments and invokes some
##' function with them.
##'
##' The idea is that to write a command line utility with R, you just
##' write a main() function use Rscript as your hashbang interpreter,
##' and at the end of your R script call run_as_command.
##'
##' TODO: Named arguments given with two dashes, GNU style, will be
##' translated into named arguments passed to the function. A bare
##' double dash means to discontinue named-argument parsing for the
##' rest of the command line.
##'
##' TODO: A help argument will be constructed according to the Roxygen
##' documentation for the function.
##'
##' @param func Which function to invoke. Defaults to whatever "main"
##' function is defined in the calling scope.
##' @param arguments The command line arguments to parse. By default,
##' uses commandArgs(trailingOnly=TRUE)
##' @param require.toplevel Only run if invoked from the top level, as
##' from Rscript.
##' @param require.noninteractive Only run if in a non-interactive R
##' session.
##' @return Nothing. Things printed will naturally go out stdout and
##' errors during execution will naturally result in a nonzero exit
##' code.
##' @author Peter Meilstrup
##' @export
run_as_command <- function(  func=parent.frame()$main
                , arguments=commandArgs(trailingOnly=TRUE)
                , require.toplevel=TRUE
                , require.noninteractive=TRUE) {
  if (     (length(sys.frames()) == 1 || !require.toplevel )
        && (!interactive() || !require.noninteractive) ) {
    do.call(func, as.list(arguments))
  }
}
