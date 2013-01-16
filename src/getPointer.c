#include <R.h>
#include <Rinternals.h>

int dots_length(SEXP dots);
const char* sexp_type_to_string(SEXPTYPE type);

/*
 * Create extract pointers or literals corresponding to every element of a
 * dots-list.
 */
SEXP expression_digest(SEXP dots) {
  SEXP result, pointers, s;
  int i;

  const int length = dots_length(dots);
  const int buflength = 30 * dotslength;
  char buf[MAX(length * 30, 2<<16)]; /*  */
  char *bufptr = buf;
  for (s = dots, i = 0; i < length; s = CDR(s), i++) {
    int done=0;
    SEXP item=CAR(s);
    while(!done) {
      switch (TYPEOF(item)) {
      case PROMSXP:
        /* if we have a promise, drill down. */
        item=PRCODE(item);
        break;
      case CHARSXP:
        /* interned string, represent its pointer */
        bufptr += snprintf(bufptr, buf + sizeof(buf) - bufptr,
                           "c%p.", CHAR(item)); break;
      case REALSXP:
      case INTSXP:
      case STRSXP:
        /* we have a code literal? represent it canonically */
        if (LENGTH(item) != 1) {
          error("literal %s with length %d?",
                sexp_type_to_string(TYPEOF(item)), LENGTH(item));
        }
        switch(TYPEOF(item)) {
        case REALSXP: bufptr += snprintf(bufptr, buf + sizeof(buf) - bufptr,
                                         "r%la.", REAL(item)[0]); break;
        case INTSXP: bufptr += snprintf(bufptr, buf + sizeof(buf) - bufptr,
                                        "i%x.", INTEGER(item)[0]); break;
        case STRSXP: bufptr += snprintf(bufptr, buf + sizeof(buf) - bufptr,
                                        "%s.", CHAR(STRING_ELT(item,0))); break;
        default: error("this should never happen");
        }
        done = 1;
        break;
      case SYMSXP:
      case LANGSXP:
      case EXPRSXP:
      case BCODESXP:
      case NILSXP:
        /* We have an expression-ish, represent its pointer. */
        bufptr += snprintf(bufptr, buf + sizeof(buf) - bufptr,
                           "e%p.", item);
        done=1;
        break;
      default:
        error("Unexpected type %s", sexp_type_to_string(TYPEOF(item)));
      }
    }

    if (bufptr >= buf + sizeof(buf)) {
      error("too many arguments to macro");
    }
    //set the name as a string
  }
  PROTECT(result = allocVector(STRSXP, 1));
  SET_STRING_ELT(result, 0, mkChar(buf));
  UNPROTECT(1);

  return(result);
}

/* measure the length of a dots object. */
int dots_length(SEXP dots) {
  SEXP s; int length;
  if (TYPEOF(dots) != DOTSXP) {
    error("Expected a dots object");
  }

  for (s = dots, length = 0; s != R_NilValue; s = CDR(s), length++) {
    if (TYPEOF(CAR(s)) != PROMSXP) {
      error("Expected PROMSXP (%d) at position %d, got type %s",
            PROMSXP, length, sexp_type_to_string(TYPEOF(CAR(s))));
    }
  }
  return length;
}

const char* sexp_type_to_string(SEXPTYPE type) {
  switch (type) {
  case NILSXP: return "NILSXP";
  case SYMSXP: return "SYMSXP";
  case LISTSXP: return "LISTSXP";
  case CLOSXP: return "CLOSXP";
  case ENVSXP: return "ENVSXP";
  case PROMSXP: return "PROMSXP";
  case LANGSXP: return "LANGSXP";
  case SPECIALSXP: return "SPECIALSXP";
  case BUILTINSXP: return "BUILTINSXP";
  case CHARSXP: return "CHARSXP";
  case LGLSXP: return "LGLSXP";
  case INTSXP: return "INTSXP";
  case REALSXP: return "REALSXP";
  case CPLXSXP: return "CPLXSXP";
  case STRSXP: return "STRSXP";
  case DOTSXP: return "DOTSXP";
  case ANYSXP: return "ANYSXP";
  case VECSXP: return "VECSXP";
  case EXPRSXP: return "EXPRSXP";
  case BCODESXP: return "BCODESXP";
  case EXTPTRSXP: return "EXTPTRSXP";
  case WEAKREFSXP: return "WEAKREFSXP";
  case RAWSXP: return "RAWSXP";
  case S4SXP: return "S4SXP";
  case NEWSXP: return "NEWSXP";
  case FREESXP: return "FREESXP";
  case FUNSXP: return "FUNSXP";
  default: error("unknown type %d", type);  
  }       
}
