#include <R.h>
#include <Rinternals.h>

int _dots_length(SEXP dots);
SEXP stringify_item(SEXP, char *);

/*
 * Extract every unevaluated element or literal of a
 * dots-list. In the names, give their pointers.
 */
SEXP _expressions_and_pointers(SEXP dots) {
  SEXP result, names, s;
  int i, length;
  length = _dots_length(dots);
  PROTECT(result = allocVector(VECSXP, length));
  PROTECT(names = allocVector(STRSXP, length));
  for (s = dots, i = 0; i < length; s = CDR(s), i++) {
    SEXP extract;
    SEXP item=CAR(s);
    char buf[99]; /* will hold a n optional tag and =, value, and |...*/
    char *bufptr = buf;
    if (TAG(s) != R_NilValue) {
      if (TYPEOF(TAG(s)) != SYMSXP)
        error("expected SYMSXP in tag, found %s",
              type2char(TYPEOF(TAG(s))));
      if (TYPEOF(PRINTNAME(TAG(s))) != CHARSXP)
        error("expected CHARSXP in symbol name, found %s",
              type2char(TYPEOF(PRINTNAME(TAG(s)))));
      bufptr += sprintf(bufptr, "c%p=", CHAR(PRINTNAME(TAG(s))));
    }
    extract = stringify_item(item, bufptr);
    SET_VECTOR_ELT(result, i, extract);
    SET_STRING_ELT(names, i, mkChar(buf));
  }

  /* set the name as a string */
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return(result);
}

/* Return some canonical interned names for each of a list of objects.
 * May reveal that objects are poor holders of things. */
SEXP _object_pointers(SEXP list) {
  SEXP names;
  int i, length;
  if (TYPEOF(list) != VECSXP)
    error("expected VECSXP in tag, found %s",
          type2char(TYPEOF(TAG(list))));
  length = LENGTH(list);
  PROTECT(names = allocVector(STRSXP, length));

  SEXP name_names;
  name_names = getAttrib(list, R_NamesSymbol);
  if (name_names != R_NilValue) {
    if (TYPEOF(name_names) != STRSXP)
      error("expected STRSXP in tag, found %s",
            type2char(TYPEOF(name_names)));
    if (LENGTH(name_names) < length)
      name_names = R_NilValue;
  }

  for (i = 0; i < length; i++) {
    SEXP item = VECTOR_ELT(list, i);
    char buf[99]; /* will hold a n optional tag and =, value, and |...*/
    char *bufptr = buf;

    if (name_names != R_NilValue) {
      SEXP name = STRING_ELT(name_names, i);
      if (name != R_BlankString)
        bufptr += sprintf(bufptr, "c%p=", R_CHAR(name));
    }

    stringify_item(item, bufptr);
    SET_STRING_ELT(names, i, mkChar(buf));
  }

  setAttrib(names, R_NamesSymbol, name_names);

  /* set the name as a string */
  UNPROTECT(1);
  return(names);
}

/* Construct some canonical pointer and put it into a char buffer.
 * Return a SEXP you might want to hold on to.
 */
SEXP stringify_item(SEXP item, char *bufptr) {
  int done = 0;
  SEXP result = R_NilValue;
  while(!done) {
    switch (TYPEOF(item)) {
    case PROMSXP:
      /* if we have a promise, drill down. */
      item=PRCODE(item);
      break;
    case CHARSXP:
      /* interned string, represent its pointer */
      bufptr += sprintf(bufptr, "c%p", CHAR(item)); break;
      result = item;
    case REALSXP:
    case INTSXP:
    case STRSXP:
    case LGLSXP:
      /* we have a code literal. represent it canonically,
         and don't hold a ref to a simple number. */
      result = R_NilValue;
      if (LENGTH(item) == 0) {
        switch(TYPEOF(item)) {
        case REALSXP: bufptr += sprintf(bufptr, "r0"); break;
        case INTSXP: bufptr +=  sprintf(bufptr, "i0"); break;
        case LGLSXP: bufptr += sprintf(bufptr, "l0"); break;
        case STRSXP: bufptr += sprintf(bufptr, "s0"); break;
        default: error("this should never happen");
        }
      } else if (LENGTH(item) == 1) {
        switch(TYPEOF(item)) {
        case REALSXP: bufptr += sprintf(bufptr, "r%la", REAL(item)[0]); break;
        case INTSXP: bufptr += sprintf(bufptr, "i%x", INTEGER(item)[0]); break;
        case LGLSXP: bufptr += sprintf(bufptr, "l%x", LOGICAL(item)[0]); break;
        case STRSXP:
          bufptr += sprintf(bufptr, "s%p", CHAR(STRING_ELT(item, 0))); break;
          result = STRING_ELT(item, 0);
        default: error("this should never happen");
        }
      } else {
        /* for longer values, represent the pointer */
        bufptr += sprintf(bufptr, "v%p", (void *)item);
        result = item;
      }
      done = 1;
      break;
    case VECSXP:
      bufptr += sprintf(bufptr, "l%p", (void *)item);
      result = item;
      done = 1;
      break;
    case SYMSXP:
    case LANGSXP:
    case EXPRSXP:
    case BCODESXP:
    case NILSXP:
      /* We have an expression-ish, represent its pointer. */
      bufptr += sprintf(bufptr, "e%p", (void *)item);
      result = item;
      done = 1;
      break;
    default:
      error("Unexpected type %s", type2char(TYPEOF(item)));
    }
  }
  SET_NAMED(result, 2);
  return result;
}

/* measure the length of a dots object. */
int _dots_length(SEXP dots) {
  SEXP s; int length;
  if (TYPEOF(dots) != DOTSXP) {
    error("Expected a dots object");
  }
  for (s = dots, length = 0; s != R_NilValue; s = CDR(s)) length++;
  return length;
}
