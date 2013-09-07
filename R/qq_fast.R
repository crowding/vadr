#a newer, faster implementation of quasiquote.

#works by compiling an expression that does that particular quasiquote,
#rather than the quasiquote iteself.

#strategy: write qq in terms of the old slow implementation of qq.
#Then replace the callbacks with functions that bootstrap themselves?

#Takes in an expression UNQUOTED. Returns the expression that would
#synthesize that expression?

#The mechanism to obtain argument names is to call back to a
#"registry" function that stores the expression and gives you a DDVAL
#ticket. This has hte side effect of informing functions whether the
#expression they are working on needs evaluation or can be quoted
#directly.

new_registry <- function() {
  counter <- 0
  expressions <- list(NULL)
  argnames <- list(NULL)
  function(expression, op="store") {
    switch(op,
           store={
             #take in an expression, record it, return a variable name.
             if (counter == length(expressions)) {
               #standard constand factor reallocation...
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

register_intercept <- function(register) {
  force(register)
  eval_needed <- FALSE
  function(expr, op="store")
      switch(op, eval_needed=eval_needed,
             {eval_needed <<- TRUE; register(expr, op)})
}

uq <- function(expr) {
  registry = new_registry()
  qq(
      ( function(
          .=...(registry(op = "argnames"))
          )
       .(uq_element(expr, registry))
       )(
           ...(registry(op = "expressions"))))
}

uq_element <- function(expr, registry) {
  if (is.pairlist(e))
      qq(as.pairlist(.(uq_seq(e, registry))))
  else if (is.call(e)) {
    if (e[[1L]] == quote(`function`) && length(e) > 3)
        e <- e[1:3] # strip srcref
    if (e[[1L]] == quote(.))
        call("list", .(registry(e[[2L]])))
    else if (e[[1L]] == quote(...) )
        as.call(c(list(quote(list))), registry(e[[2L]]))
    else
        qq( as.call( .( uq_seq(e, registry) )) )
  } else if (!is.recursive(e)) {
    if (is.name(e))
        uq_name(e, registry)
    else
        e
  } else
      uq_seq(e, registry)
}

uq_pairlist <- function(l, register) {
  contains_unquote <- FALSE
  register2 <- function(expr) {
    contains_unquote <<- TRUE
    register(expr)
  }
  uq <- uq_seq(l)
  if (contains_unquote)
      l #directly return the pairlist.
  else
      call("as.pairlist", uq)
}

#Returns an expression that makes a list
uq_seq <- function(l, register) {
  contains_unquote <- vector("logical", length(l))
  register2 <- function(expr) {
    contains_unquote[i] <<- TRUE
    register(expr)
  }
  l <- unquote
  register2 = function(expr) {
    eval_needed <<- TRUE;
    register(expr)
  }

  for (i in 1:length(l)) {
    uq_element(uq_l, register2)
  }

  if(any(contains_unquote)) {
    stop("No")
  } else as.list(x) #list
}

#unquote a single name...
uq_name <- function(name, register) {
  register <- register_intercept(register)
  ch <- uq_char(as.character(name), register)
  if (register(op="eval_needed"))
      ch[[1]] <- quote(as.name)
  else
      ch <- as.name(ch)
  ch
}

#unquote a single char...
uq_char <- function(ch, register) {
  match <- str_match(ch, "^\\.\\((.*)\\)$")
  if (!is.na(match[2])) {
    expr <- parse(text=match[2])[[1]]
    if (is.language(expr)) {
      call("as.character", register(expr))
    } else {
      as.character(expr)
    }
  } else {
    ch
  }
}
