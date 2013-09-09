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


#YOU RETURN LIST OF EVALUABLES THAT_EVAL_TO_LIST
uq.name <- function(expr, register) {
  register <- register_intercept(register)
  ch <- uq(as.character(expr), register)
  if (register(op="eval_needed")) {
    ch[[1]][[2]][[1]] <- quote(as.name)}
  else
      ch <- list(list(as.name(ch[[1]][[1]])))
  ch
}

#unquote a single char...
uq.character <- function(expr, register) {
  match <- str_match(expr, "^\\.\\((.*)\\)$")
  if (!is.na(match[2])) {
    expr <- parse(text=match[2])[[1]]
    if (is.language(expr)) {
      list(call("list", call("as.character", register(expr))))
    } else {
      list(list(as.character(expr)))
    }
  } else {
    list(expr)
  }
}

#called with call objects of form ".()" or "...()"
#RETURN a list of evaluables
uq_dots <- function(expr, register) {
  lapply(expr[-1], function(unquotable) {
      if (is.language(unquotable)) {
        register(unquotable)
      } else {
        unquotable
      }
    })
}

#RETURN_A_LIST_OF_EVALUABLES_THAT_EVAL_TO_LISTS
uq.call <- function(expr, register) {
  register <- register_intercept(register)
  if (length(expr) >= 1 && expr[[1]] == quote(...)) {
    unquoted <- uq_dots(expr, register)
    if (register(op="eval_needed")) {
      list(as.call(c(list(quote(c)), unquoted)))
    } else {
      list(do.call(c, unquoted))        #pre-evaluate
    }
  } else if (length(expr) >= 1 && expr[[1]] == quote(.)) {
    unquoted <- uq_dots(expr, register)
    if (register(op="eval_needed")) {
      list(as.call(c(list(quote(list)), unquoted)))
    } else {
      do.call(list, unquoted) #pre-evaluate
    }
  } else {
    unquoted <- uq(as.list(expr), register)
    if (register(op="eval_needed")) {
      as.call(c(quote(as.call), unquoted))
    } else {
      #this does have to be based on unquoted, because .() need stripped.
      #but we eval the args and return literal list containing the call object.
      list(list(as.call(do.call(c, unquoted))))
    }
  }
}
#Other syntax elements have different S3 classes but are just calls
`uq.(` <- uq.call
`uq.{` <- uq.call
`uq.while` <- uq.call
`uq.for` <- uq.call
`uq.if` <- uq.call

#DOES_THIS_RETURN A_LIST OF EVALUABLES? THAT EACH_EVAL_TO_LISTS?
uq.list <- function(expr, register) {
  needs_eval <- vector("logical", length(expr))
  register_old <- register
  register <- function(expr, op="store") {
    needs_eval[i] <<- i
    register_old(expr, op)
  }
  unquoted <- vector("list", length(expr))
  for(i in 1:length(expr)) {
    unquoted[[i]] <- uq(expr[[i]], register)
  }
  if (any(needs_eval) >= 1) {
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
    list(as.call(c %()% c(list(quote(c)), unquoted)))
  } else {
    list(expr) #just quote
  }
}

uq.default <- function(expr, register) {
  if (is.language(expr)) {
    stop("Language object should have been covered by another class.")
  } else {
    list(expr)
  }
}

#General rule: everything returns a LIST.
