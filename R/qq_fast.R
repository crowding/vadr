#a newer, faster implementation of quasiquote.

#works by compiling a function that does that particular quasiquote,
#rather than walking over the list every time.

#Unquote core. Take an expression, return a list of expressions that
#when evaluated make the unquoted expression.
uq <- function(expr, register) UseMethod("uq")

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

#Unquote methods sometimes need to know if subexpressions need to be
#evaluated (or if they can just be quoted literally.)
register_intercept <- function(register) {
  force(register)
  eval_needed <- FALSE
  function(expr, op="store")
      switch(op, eval_needed=eval_needed,
             {eval_needed <<- TRUE; register(expr, op)})
}

#All unquote methods return objects that _when evaluated_ produce the
#value enclosed in a _list_ This is first so that unquote-splicing
#works straightforwardly (just return a longer list), second so that
#unquoted parts can just be literal lists catenated on.

#unquote a single expr...
uq.name <- function(expr, register) {
  register <- register_intercept(register)
  ch <- uq(as.character(expr), register)
  if (register(op="eval_needed"))
      ch[[2]][[1]] <- quote(as.name)
  else
      ch <- list(as.name(ch[[1]]))
  ch
}

#unquote a single char...
uq.character <- function(expr, register) {
  match <- str_match(expr, "^\\.\\((.*)\\)$")
  if (!is.na(match[2])) {
    expr <- parse(text=match[2])[[1]]
    if (is.language(expr)) {
      call("list", call("as.character", register(expr)))
    } else {
      list(as.character(expr))
    }
  } else {
    list(expr)
  }
}

uq.call <- function(expr, register) {
  #if an unquote is found, returns expressions that produces a _list_.
  if (length(expr) >= 2 && expr[[1]] == quote(...)) {
    unquotable <- expr[[2]]
    if (is.language(unquotable)) {
      register(unquotable)
    } else {
      unquotable
    }
  } else if (length(expr) >= 2 && expr[[1]] == quote(.)) {
    unquotable <- expr[[2]]
    if (is.language(unquotable)) {
      call("list", register(unquotable))
    } else {
      as.list(unquotable)
    }
  } else {
    register <- register_intercept(register)
    unquoted <- uq(as.list(expr), register)
    if (register(op="eval_needed")) {
      list(call("as.call", unquoted))
    } else {
      list(expr)
    }
  }
}
`uq.(` <- uq.call
`uq.{` <- uq.call
`uq.while` <- uq.call
`uq.for` <- uq.call
`uq.if` <- uq.call

uq.list <- function(expr, register) {
  needs_eval <- vector("logical", length(expr))
  register_old <- register
  register <- function(expr) {
    needs_eval[i] <<- i
    register_old(expr)
  }
  unquoted <- vector("list", length(expr))
  for(i in 1:length(expr)) {
    unquoted[[i]] <- uq(expr[[i]], register)
  }
  if (any(needs_eval) >= 1) {
    #try to somewhat efficiently pack in quoted values
    #by pre-concatenating adjacent literal values
    runs <- rle(needs_eval)
    runs$first_index <- (c(0, cumsum(runs$lengths)) + 1)[1:length(runs[[1]])]
    args <- mapply(
        runs$lengths, runs$values, runs$first_index,
        FUN=function(len, value, first) {
          if (value == 0) {
            do.call(`c`, unquoted[seq(first, length.out=len)])
          } else {
            #length has to be 1 by construction
            unquoted[[first]]
          }
        })
    as.call(c(list(quote(c)), args))
  } else {
    do.call(c, unquoted)
  }
}


#General rule: everything returns a LIST.
