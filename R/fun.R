##' A very compact way to define a function.
##'
##' \code{fun} captures its first argument unevaluated and turns it
##' into a function. Every name used in the expression becomes an
##' argument, unless it looks like a function call. If you don't
##' intend to capture a particular variable, you can not provide it,
##' and it will use a default value that pulls from the enclosing
##' scope. For example:
##'
##' \code{ > f <- fun(x/y)
##' > f(10,2)
##' [1] 5
##' > f
##' function (x = eval(quote(x),<environment>),
##'           y = eval(quote(y),<environment>) {
##'   x/y
##' }
##'
##' "\code{fun}" is used with "\code{dm_ply}" the way that "\code{with}" is
##' used with  "\link[plyr]{d_ply}".
##'
##' "\code{...}" is supported in the function definitions and should
##' behave as you expect.
##'
##' @param expr The expression to use as the function's body.
##'
##' @param .all.names Defaults to \code{FALSE}, in which case the
##' formal arguments of the function are only the parts of the
##' expression that "look like" variables (i.e. names that do not head
##' calls). If set to \code{TRUE}, all symbols are wrapped in formal
##' arguments. This includes all things that R treats as calls, like
##' \code{`+`}, \code{`(`} and \code{`{`}. Note that "in the order of
##' appearance" means in the s-expression order, so that
##' \code{"fun(x+y, .all.names=TRUE)"} will have arguments named "+",
##' "x" and "y" in that order.
##'
##' @return A newly constructed function.
##'
##' @note Since it doesn't know which symbols you intend to be
##' arguments and which you intend to take from the enclosing
##' environment, it captures all symbols in defaults; therefore it
##' won't work as a closure that reflects changes in the enclosing
##' environment.
##'
##' @author Peter Meilstrup
##'
##' @export
fun <- macro(function(body, .all.names=FALSE) {
  names <- all.names(body, functions=.all.names, unique=TRUE)
  arglist <- qqply(`.(name)`=eval(quote(`.(name)`), ..fun_envir))(name=names)
  qq(
    (function(..fun_envir) {
      function(.=..(arglist)) .(body)
    })(
      .(environment)()
    )
  )
})
