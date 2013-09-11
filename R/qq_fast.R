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
#value enclosed in a _list_ This is first so that unquote-splicing
#works straightforwardly (just return a longer list), second so that
#unquoted parts can just be literal lists catenated on.

#YOU RETURN LIST OF EVALUABLES THAT_EVAL_TO_LISTS
uq.name <- function(expr, register) {
  register <- register_intercept(register)
  ch <- uq(as.character(expr), register)
  if (register(op="eval_needed")) {
    ch[[1]][[2]][[1]] <- quote(uq_as_name)}
  else
      ch <- list(list(uq_as_name(ch[[1]][[1]])))
  ch
}

uq_as_name <- function(x)
    if (x == "") quote(expr=) else as.name(x)

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
    list(list(expr))
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
#"." wraps each elmeent in a list. "..." lets them catenate.
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
      #lapply(lapply(unquoted, as.list), do.call, what=list) #pre-eval each
      list(do.call(list, unquoted))     #pre-evaluate
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
  names(unquoted) <- names(expr)
  for(i in 1:length(expr)) {
    unquoted[[i]] <- uq_named(expr[i], register)
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
    list(do.call(c, as.list(c %()% unquoted))) #eval eagerly
  }
}

uq.pairlist <- function(expr, register) {
  register <- register_intercept(register)
  unquoted <- uq(as.list(expr), register)
  if (register(op="eval_needed")) {
    list(call("list", as.call(c(list(quote(as.pairlist)), unquoted))))
  } else {
    list(list(as.pairlist(do.call(c, unquoted))))
  }
}

uq.default <- function(expr, register) {
  if (is.language(expr)) {
    stop("Language object should have been covered by another method!")
  } else {
    list(expr)
  }
}

#unlike the others, this returns a single item that evals to list.
uq_named <- function(expr, register) {
  #this unquotes a single element with possible names.
  if (is.null(names(expr))) return(uq(expr[[1]], register))
  eval.name <- FALSE
  unquoted.name <- uq(
      names(expr), function(expr, op="store") {
        eval.name <<- TRUE; register(expr, op)})[[1]]
  eval.value <- FALSE
  unquoted.value <- uq(expr[[1]], function(expr, op="store") {
    eval.value <<- TRUE; register(expr, op)})[[1]]
  #We have two lists of items that evaluate to lists.
  if (eval.value) {
    if (eval.name) {
      print("wat1")
      calling("structure",
              c(calling("c", unquoted.value),
                names=calling("c", unquoted.name)))
      do.call("structure", calling("c", c(unquoted.value, names=unquoted.name)))
    } else {
      print("wat2")
      call("structure", calling("c", unquoted.value),
           names=literal(do.call("c", unquoted.name)))
    }
  } else if (eval.name) {
    print("wat3")
    call("structure",
         literal(do.call("c", unquoted.value)),
         names=calling("c", unquoted.name))
  } else {
    print("wat4")
    literal(do.call(structure,
                    list(calling("c", unquoted.value),
                         names=calling("c", unquoted.name))))
  }
}

literal <- function(expr) {
  if (is.call(expr) | is.name(expr)) {
    call("quote", expr)
  } else {
    expr
  }
}

calling <- function(f, args) {
  #args is a list of things that can each be eval'ed to produce lists
  c(eval, as.call(as.name(f)), args())
}
