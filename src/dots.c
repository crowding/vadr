#include <R.h>
#include <Rinternals.h>

int dots_length(SEXP dots);

SEXP dots_unpack(SEXP dots) {
  int i;
  SEXP s;
  SEXP item;
  int length = 0;
  SEXP names, environments, expressions, values;
  //SEXP evaluated, codeptr, missing, wraplist;
  //SEXP seen;

  SEXP dataFrame;
  SEXP colNames;
  SEXP class;

  //check inputs and measure length
  length = dots_length(dots);

  // unpack information for each item:
  // names, environemnts, expressions, values, evaluated, seen
  PROTECT(names = allocVector(STRSXP, length));
  PROTECT(environments = allocVector(VECSXP, length));
  PROTECT(expressions = allocVector(VECSXP, length));
  PROTECT(values = allocVector(VECSXP, length));

  for (s = dots, i = 0; s != R_NilValue; s = CDR(s), i++) {
    SEXP item = CAR(s);
    
    // if we have an unevluated promise whose code is another promise, descend
    while ((PRENV(item) != R_NilValue) && (TYPEOF(PRCODE(item)) == PROMSXP)) {
      item = PRCODE(item);
    }

    SET_STRING_ELT(names, i, isNull(TAG(s)) ? mkChar("") : asChar(TAG(s)));
    SET_VECTOR_ELT(environments, i, PRENV(item));
    SET_VECTOR_ELT(expressions, i, PRCODE(item));

    if (PRVALUE(item) != R_UnboundValue) {
      SET_VECTOR_ELT(values, i, PRVALUE(item));
    } else {
      SET_VECTOR_ELT(values, i, R_NilValue);
    }
  }

  PROTECT(dataFrame = allocVector(VECSXP, 4));
  SET_VECTOR_ELT(dataFrame, 0, names);
  SET_VECTOR_ELT(dataFrame, 1, environments);
  SET_VECTOR_ELT(dataFrame, 2, expressions);
  SET_VECTOR_ELT(dataFrame, 3, values);

  PROTECT(colNames = allocVector(STRSXP, 4));
  SET_STRING_ELT(colNames, 0, mkChar("name"));
  SET_STRING_ELT(colNames, 1, mkChar("envir"));
  SET_STRING_ELT(colNames, 2, mkChar("expr"));
  SET_STRING_ELT(colNames, 3, mkChar("value"));

  setAttrib(expressions, R_ClassSymbol, ScalarString(mkChar("deparse")));
  setAttrib(environments, R_ClassSymbol, ScalarString(mkChar("deparse")));
  setAttrib(values, R_ClassSymbol, ScalarString(mkChar("deparse")));

  setAttrib(dataFrame, R_NamesSymbol, colNames);
  setAttrib(dataFrame, R_RowNamesSymbol, names);
  setAttrib(dataFrame, R_ClassSymbol, ScalarString(mkChar("data.frame")));

  UNPROTECT(6);
  return(dataFrame); 
}

SEXP dots_names(SEXP dots) {
  SEXP names, s;
  int i, length;

  length = dots_length(dots);

  PROTECT(names = allocVector(STRSXP, length));

  for (s = dots, i = 0; s != R_NilValue; s = CDR(s), i++) {
    SEXP item = CAR(s);
    SET_STRING_ELT(names, i, isNull(TAG(s)) ? mkChar("") : asChar(TAG(s)));
  }
  UNPROTECT(1);
  return(names);
}

SEXP _getName(SEXP names, int i) {
  /* return  names[i]  if it is a character (>= 1 char), or NULL otherwise */
    if (names != R_NilValue &&
        STRING_ELT(names, i) != R_NilValue &&
        CHAR(STRING_ELT(names, i))[0] != '\0') /* length test */
        return STRING_ELT(names, i);
    else
        return R_NilValue;
}

SEXP call_function_from_dots(SEXP fun, SEXP args, SEXP envir, SEXP unpromise) {
  if (!isEnvironment(envir))
    error("'envir' must be an environment");
  if (TYPEOF(args) != DOTSXP)
    error("Expected a DOTSXP, got %s", type2char(TYPEOF(args)));
  if (!isLogical(unpromise) || length(unpromise) != 1)
    error("Expected a scalar logical for unforce, got %s[%d]",
          type2char(TYPEOF(unpromise)), length(unpromise));
  int do_unpromise = LOGICAL(unpromise)[0];

  SEXP call;
  PROTECT( call = allocVector(LANGSXP, length(args) + 1 ));
  SETCAR(call, fun);

  SEXP in, out, name;
  int i;
  for (out = CDR(call), in = args, i = 0;
       out != R_NilValue;
       in=CDR(in), out=CDR(out), i++) {
    SETCAR( out, CAR(in));
    if (do_unpromise && TYPEOF(CAR(out)) == PROMSXP) {
      // unwrap multiple-promise chains, leaving one promise that
      // "looks" evaluated.
      SEXP bot = CAR(out);
      while (1) {
        if (PRENV(bot) == R_NilValue) {
          break;
        } else {
          if (TYPEOF(PRCODE(bot)) == PROMSXP) {
            bot = PRCODE(bot);
          } else break;
        }
      }
      if (PRENV(bot) != R_NilValue) {
        if (PRVALUE(bot) == R_UnboundValue) {
          SET_PRVALUE(bot, PRCODE(bot));
        }
        SET_PRENV(bot, R_NilValue);
      }
      SETCAR(out, bot);
    }
    SET_TAG( out, TAG(in) );
  }
  SEXP result = eval( call, envir );

  UNPROTECT(1);
  return result;
}

