#include <R.h>
#include <Rinternals.h>

int dots_length(SEXP dots);
const char* sexp_type_to_string(SEXPTYPE type);

/*
 * Extract every unevaluated element or literal of a
 * dots-list. In the names, give their pointers.
 */
SEXP expressions_and_pointers(SEXP dots) {
  SEXP result, pointers, s;
  int i, length;

  length = dots_length(dots);
  PROTECT(result = allocVector(VECSXP, length));
  PROTECT(pointers = allocVector(STRSXP, length));
  for (s = dots, i = 0; i < length; s = CDR(s), i++) {
    int done=0;
    SEXP item=CAR(s);
    char buf[99];
    while(!done) {
      switch (TYPEOF(item)) {
      case PROMSXP:
        /* if we have a promise, drill down. */
        item=PRCODE(item);
        break;

      case CHARSXP:
        /* interned string, represent its pointer */
        SET_VECTOR_ELT(result, i, item);
        sprintf(buf, "c%p", CHAR(item)); break;
        SET_STRING_ELT(pointers, i, mkChar(buf));

      case REALSXP:
      case INTSXP:
      case STRSXP:
        /* we have a code literal? represent it canonically, 
           and don't hold a ref to a simple number. */
        SET_VECTOR_ELT(result, i, R_NilValue);
        if (LENGTH(item) != 1) {
          error("unexpected literal %s with non-unitary length %d",
                sexp_type_to_string(TYPEOF(item)), LENGTH(item));
        }
        switch(TYPEOF(item)) {
        case REALSXP: sprintf(buf, "r%la", REAL(item)[0]); break;
        case INTSXP: sprintf(buf, "i%x", INTEGER(item)[0]); break;
        case STRSXP: 
          /* on the other hand, do hold a ref to interned string literals. */
          sprintf(buf, "s%p", CHAR(STRING_ELT(item,0))); 
          SET_VECTOR_ELT(result, i, STRING_ELT(item,0));
          break;
        default: error("this should never happen");
        }
        SET_STRING_ELT(pointers, i, mkChar(buf));
        done = 1;
        break;

      case SYMSXP:
      case LANGSXP:
      case EXPRSXP:
      case BCODESXP:
      case NILSXP:
        /* We have an expression-ish, represent its pointer. */
        SET_VECTOR_ELT(result, i, item);
        sprintf(buf, "e%p", item);
        SET_STRING_ELT(pointers, i, mkChar(buf));
        done=1;
        break;
      default:
        error("Unexpected type %s", sexp_type_to_string(TYPEOF(item)));
      }
    }
  }

  /* set the name as a string */
  setAttrib(result, R_NamesSymbol, pointers);
  UNPROTECT(2);

  return(result);
}

/* measure the length of a dots object. */
int dots_length(SEXP dots) {
  SEXP s; int length;
  if (TYPEOF(dots) != DOTSXP) {
    error("Expected a dots object");
  }
  for (s = dots, length = 0; s != R_NilValue; s = CDR(s)) length++;
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
