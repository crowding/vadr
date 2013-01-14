#include <R.h>
#include <Rinternals.h>

SEXP getPointer(SEXP args) {
  SEXP result;
  SEXP s;
  int i;
  int length = 0;

  // count the sexps
  switch (TYPEOF(args)) {
  case LISTSXP:
    for (s = CDR(args); s != R_NilValue; s = CDR(s)) {
      length++;
    }
    break;
  default:
    return R_NilValue;
  }

  PROTECT(result = allocVector(INTSXP, length));
  for (s = CDR(args), i = 0; s != R_NilValue; s = CDR(s), i++) {
    INTEGER(result)[i] = (long int)CAR(s);
  }
  UNPROTECT(1);

  return(result);
}
