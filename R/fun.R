##' A compact way to define a function.
##'
##' \code{fun} captures its first argument unevaluated and turns it
##' into a function. Every name used in the expression becomes an
##' argument, unless it looks like a function call. If you don't
##' intend to capture a particular variable, you can not provide it,
##' and it will use a default value that pulls from the enclosing
##' scope.
##'
##' "\code{...}" is supported in the function definitions and should
##' behave as you expect.
##'
##' @usage fun(expr, .all.names=FALSE)
##'
##' @param expr The expression to use as the function's body.
##'
##' @param .all.names Whether to include call heads as parameters.
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
##' @examples
##' f <- fun(x/y)
##' f(10,2)  # prints 5
##' f
##' # function (x = eval(quote(x),..fun_envir),
##' #           y = eval(quote(y),..fun_envir) {
##' #   x/y
##' # }

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
