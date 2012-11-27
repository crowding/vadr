
daccumulator <- function() {
  colnames <- character(0)
  next_row <- 1
  num_rows <- 0
  expansion_factor <- 3/2

  put <- function(...) {
    data <- list(...)
    accumulator <<- rebuild_accumulator(names(data),
                                        vapply(data, storage.mode, ""))
    accumulator(...)
  }

  storage <- new.env(parent=globalenv())
  thunks <- new.env(parent=storage)

  add_column <- function(name, mode) {
    #this actually allocates twice. I'll have to have a way to copy
    #and inspect one of the arguments to find its mode.
    storage[[name]] <- logical(num_rows)
    eval(envir=thunks,
         template( .(as.name(name)) <-
                  function(data, indices) .(as.name(name))[indices] <<- data
                  )
         )
  }

  grow <- function(n) {
    if (next_row + n > num_rows) {
      new_size <- ceil( (next_row + num_rows) * expansion_factor )
    }
  }

  rebuild_accumulator <- function(new.colnames, storage.mode) {
    added.cols <- setdiff(colnames, colnames)
    all.colnames <- union(added.cols, colnames)

    colnames <<- all.colnames
    put <<- eval(template({
      function( .=...( templapply( .(missing.value()), n=all.colnames ) ), ... ) {
        if (length(substitute(alist(...)) > 0)) {
          #force the promises to get their data types.
          data <- list(...)
          put <<- new_accumulator(names(data), )
          put( ...( mapply(as.name, all.colnames) ), ... )
        } else {
          grow(n_new_rows)
          indices <- seq(next_row, n_new_rows)
          ...( templapply( colname=all.colnames,
                           thunks$`.(col)`(.(as.name(col)),indices) ) )
          next_row <<- next_row + n_new_rows
        }
      }
    }))
  }
}
