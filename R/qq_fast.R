#a newer, faster implementation of quasiquote.

#works by compiling a function that does that particular quasiquote,
#rather than walking over the list every time.

#Unquote core. Take an expression, return an object that when evaluated
#produces the unquoted expression wrapped in a list.
uq <- function(expr, register) {
  if (missing(expr)) quote(list(quote(expr=)))
  else UseMethod("uq")
}

#Unquoting functions walk over a parse tree and use a callback to
#obtain argument names substituting out sections that need to be
#evaluated in the caller. The callbacks also let nested sections know
#if they need to evaluate contents, or if they can get away with
#literal quoting.
new_registry <- function() {
  counter <- 0
  expressions <- vector("list", 16)
  argnames <- vector("list", 16)
  function(expression, op="store") {
    switch(op,
           store={
             #take in an expression, record it, return a variable name.
             if (counter == length(expressions)) {
               #constant factor reallocation
               length(expressions) <<- 2*length(expressions)
               length(argnames) <<- 2*length(argnames)
             }
             counter <<- counter+1
             argname <- as.name(paste0("..", counter))
             expressions[[counter]] <<- structure(expression)
             argnames[[counter]] <<- as.character(argname)
             argname
           },
           expressions={
             structure(expressions[seq_len(counter)],
                       names=argnames[seq_len(counter)])
           })
  }
}

#Unquote methods might like to know if subexpressions have any
#unquotes in them evaluated If not, they can optimize by
#pre-evaluating those pieces. So they use a flag on the registry to
#pick these up.
register_intercept <- function(register) {
  force(register)
  eval_needed <- FALSE
  function(expr, op="store")
      switch(op, eval_needed=eval_needed,
             {eval_needed <<- TRUE; register(expr, op)})
}

#All unquote methods return objects that _when evaluated_ produce the
#expression(s) enclosed in a _list_.

#unquote a single char...
#YOU RETURN A SINGLE EVALUABLE THAT EVALS TO A LIST
uq.character <- function(expr, register) {
  match <- regexec("^\\.\\((.*)\\)$", expr)
  if (any(match[[1]] >= 0)) {
    expr <- parse(text=substring(
        expr,
        match[[1]][[2]],
        match[[1]][[2]] + attr(match[[1]], "match.length")[[2]] - 1))[[1]]
     if (is.language(expr)) {
      call("list", call("as.character", register(expr)))
    } else {
      list(as.character(expr))
    }
  } else {
    list(expr)
  }
}

uq_character_nolist <- function(expr, register) {
  match <- regexec("^\\.\\((.*)\\)$", expr)
  if (any(match[[1]] >= 0)) {
    expr <- parse(text=substring(
        expr,
        match[[1]][[2]],
        match[[1]][[2]] + attr(match[[1]], "match.length")[[2]] - 1))[[1]]
    if (is.language(expr)) {
      call("as.character", register(expr))
    } else {
      as.character(expr)
    }
  } else {
    expr
  }
}

uq.name <- function(expr, register) {
  register <- register_intercept(register)
  ch <- uq(as.character(expr), register)
  if (register(op="eval_needed"))
      ch <- call("list", call("uq_as_name", ch[[2]]))
  else
      ch <- literal(list(do.call("uq_as_name", ch)))
  ch
}

uq_as_name <- function(x)
    if (x == "") quote(expr=) else as.name(x)

#called with the argument of a form ".()" or "..()"
#RETURN the SIMPLE, SINGLE evaluable (doesn't eval to a list)
uq_dots <- function(expr, register) {
  if(is.language(expr))
      register(expr)
  else
      literal(expr)
}

#because CRAN will complain if I do quote(...)
splice.symbols <- lapply(c("..", "..."), as.name)

#Takes a call object
#Returns something that evals to unquoted version wrapped in a list
uq.call <- function(expr, register) {
  register <- register_intercept(register)
  if (length(expr) >= 1 && (   expr[[1]] == splice.symbols[[1]]
                            || expr[[1]] == splice.symbols[[2]])) {
    unquoted <- uq_dots(expr[[2]], register)
    if (register(op="eval_needed"))
        unquoted
    else
        literal(eval(unquoted))
  } else if (length(expr) >= 1 && expr[[1]] == quote(.)) {
    unquoted <- uq_dots(expr[[2]], register)
    if (register(op="eval_needed"))
        call("list", unquoted)
    else
        literal(list(eval(unquoted)))   #pre-evaluate
  } else if (expr[[1]] == quote(`function`)) {
    args <- uq_pairlist(expr[[2]], register)
    body <- uq(expr[[3]], register)
    if (register(op="eval_needed"))
        call("list", call("do.call", "function",
                          call("c", call("list", args), body)))
    else
        list(as.call(c(list(quote(`function`)), list(eval(args)), body)))
  } else {
    unquoted <- uq_call_args(as.list(expr), register)
    if (register(op="eval_needed"))
        call("list", call("as.call", unquoted))
    else
        list(as.call(eval(unquoted)))
  }
}
#Other syntax elements have different S3 classes but are just calls
`uq.(` <- uq.call
`uq.{` <- uq.call
`uq.while` <- uq.call
`uq.for` <- uq.call
`uq.if` <- uq.call

#Takes list of quoteds
#Returns something the evals to a pairlist.
uq_pairlist <- function(expr, register) {
  register <- register_intercept(register)
  unquoted <- uq_call_args(as.list(expr), register)
  if (register(op="eval_needed")) {
    call("as.pairlist", unquoted)
  } else {
    literal(as.pairlist(eval(unquoted)))
  }
}

#Unquoting for argument lists (including the head).
#Takes a list of quoteds,
#returns something that that evals to the whole argument list.
uq_call_args <- function(expr, register) {
  needs_eval <- vector("logical", length(expr))
  reregister <- function(expr, op="store") {
    needs_eval[i] <<- i
    register(expr, op)
  }
  unquoted <- vector("list", length(expr))
  for(i in 1:length(expr)) {
    unquoted[[i]] <- list(uq_named(expr[i], reregister))
  }
  unquoted <- do.call("c", unquoted)
  if (any(needs_eval)) {
    ## #try to somewhat efficiently pack in quoted values
    ## #by pre-concatenating adjacent literal values
    ## runs <- rle(needs_eval)
    ## runs$first_index <- (c(0, cumsum(runs$lengths)) + 1)[1:length(runs[[1]])]
    ## args <- mapply(
    ##     runs$lengths, runs$values, runs$first_index,
    ##     FUN=function(len, value, first) {
    ##       if (value == 0) {
    ##         expr[seq(first, length.out=len)] #these parts quote literally
    ##       } else {
    ##         #length has to be 1 by construction
    ##         unquoted[[first]]
    ##       }
    ##     })
    ## list(as.call(c(list(quote(c)), args))) #catenate
    as.call(c(quote(c), unquoted))
  } else {
    literal(do.call("c", unquoted))
  }
}

uq.default <- function(expr, register) {
  if (is.language(expr)) {
    stop("Language object should have been covered by another method!")
  } else {
    list(expr)
  }
}

#Takes a list of length 1. Returns evalable (not to a list.)
uq_named <- function(expr, register) {
  #this unquotes a single element with possible names.
  name <- names(expr)
  expr <- expr[[1]]
  if (is.null(name) || all(name == "")) return(uq(expr, register))

  #browser()
  eval.name <- FALSE
  unquoted.name <- uq_character_nolist(
      name, function(expr, op="store") {eval.name <<- TRUE; register(expr, op)})

  eval.value <- FALSE
  unquoted.value <- uq(
      expr, function(expr, op="store") {eval.value <<- TRUE; register(expr, op)})

  #We have two lists of items that evaluate to lists.
  if (eval.value) {
    if (eval.name) {
      call("inner_dominating_names",
           unquoted.value,
           unquoted.name)
    } else {
      call("inner_dominating_names",
           unquoted.value,
           literal(unquoted.name))
    }
  } else if (eval.name) {
    call("inner_dominating_names",
         literal(eval(unquoted.value)),
         unquoted.name)
  } else {
    literal(do.call("inner_dominating_names",
                    list(eval(unquoted.value),
                         eval(unquoted.name))))
  }
}

literal <- function(expr) {
  if (is.call(expr) | is.name(expr)) {
    call("quote", expr)
  } else {
    expr
  }
}

inner_dominating_names <- function(value, name) {
  if (is.null(names(value)) || all(names(value) == ""))
      structure(value, names=name)
  else value
}
