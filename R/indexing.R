index.data.frame <- function(df, row = 1:nrow(df), col=NULL, value=c()) {
  ##Index a data frame, puling out selected rows and columns
  ix <- index.data.frame.core(df, row, col)
  length(value) <- max(0, vapply(ix$indices.out, max, 0))
  
  mapply(function(i, o, c) {
    value[o] <<- df[i, c]
  }, ix$indices.in, ix$indices.out, ix$columns)
  value
}

index.data.frame.core <- function(df, row, col){
  if (is.null(col) && length(dim(row)) >= 2) {
    col <- row[,ncol(row)]
    row <- row[,1:ncol(row)-1]
  }

  row <- mkmatch(row, rownames(df))
  col <- mkmatch(col, colnames(df))
  indices.out <- tapply(1:length(col), col, identity)
  indices.in <- lapply(indices.out, function(x) row[x])
  columns <- vapply(indices.out, function(i) col[i[1]], 0)
  list(indices.out=indices.out, indices.in=indices.in, columns=columns)
}

             
mkmatch <- function(index, names) {
 if (is.character(index) || is.factor(index)) {
   matched <- pmatch(index, names, duplicates.ok = TRUE)
   if (any(is.na(matched) & !is.na(index))) {
     stop("Unknown indices")
   }
 } else {
   matched <- index
 }
   matched
}

index <- function(obj, ...) {
  UseMethod("index")
}

`index<-` <- function(obj, ...) {
  UseMethod("index<-")
}

`index<-.data.frame` <- function(df, row = 1:nrow(df), col=NULL, value=NULL) {
  ix <- index.data.frame.core(df, row, col)
  mapply(function(i, o, c) {
    df[i, c] <<- value[o]
  }, ix$indices.in, ix$indices.out, ix$columns)
  df
}
