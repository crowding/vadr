#include <R.h>
#include <Rinternals.h>

int dots_length(SEXP dots);

SEXP dots_info(SEXP dots) {
  int i;
  SEXP s;
  SEXP item;
  int length = 0;
  SEXP names, environments, expressions, values;
  SEXP evaluated, codeptr, missing, wraplist;
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
  PROTECT(evaluated = allocVector(LGLSXP, length));
  PROTECT(values = allocVector(VECSXP, length));
  //PROTECT(seen = allocVector(LGLSXP, length));

  //codeptr gives the pointer to the s-expression, I investigate this
  //temporarily.
  PROTECT(codeptr = allocVector(INTSXP, length));

  for (s = dots, i = 0; s != R_NilValue; s = CDR(s), i++) {
    SEXP item = CAR(s);

    SET_STRING_ELT(names, i, isNull(TAG(s)) ? mkChar("") : asChar(TAG(s)));

    //wrap, because this prevent print.data.frame from getting unfriendly :(
    PROTECT(wraplist = allocVector(VECSXP, 1));
    SET_VECTOR_ELT(wraplist, 0, PRENV(item));
    SET_VECTOR_ELT(environments, i, wraplist);
    UNPROTECT(1);

    PROTECT(wraplist = allocVector(VECSXP, 1));
    SET_VECTOR_ELT(wraplist, 0, PRCODE(item));
    SET_VECTOR_ELT(expressions, i, wraplist);
    UNPROTECT(1);

    PROTECT(wraplist = allocVector(VECSXP, length));
    SET_VECTOR_ELT(wraplist, 0, PRENV(item));
    SET_VECTOR_ELT(environments, i, wraplist);
    UNPROTECT(1);
    INTEGER(codeptr)[i] = (long int) PRCODE(item);

    if (PRVALUE(item) != R_UnboundValue) {
      LOGICAL(evaluated)[i] = 1;
      SET_VECTOR_ELT(values, i, PRVALUE(item));
    } else {
      LOGICAL(evaluated)[i] = 0;
      SET_VECTOR_ELT(values, i, R_NilValue);
    }

    //LOGICAL(seen)[i] = (PRSEEN(item) ? TRUE : FALSE);
  }

  PROTECT(class = allocVector(STRSXP, 1));
  SET_STRING_ELT(class, 0, mkChar("AsIs"));
  setAttrib(expressions, R_ClassSymbol, class);
  setAttrib(environments, R_ClassSymbol, class);
  setAttrib(values, R_ClassSymbol, class);
  UNPROTECT(1);

  PROTECT(dataFrame = allocVector(VECSXP, 6));
  PROTECT(colNames = allocVector(STRSXP, 6));
  SET_VECTOR_ELT(dataFrame, 0, names);
  SET_VECTOR_ELT(dataFrame, 1, environments);
  SET_VECTOR_ELT(dataFrame, 2, expressions);
  SET_VECTOR_ELT(dataFrame, 3, evaluated);
  SET_VECTOR_ELT(dataFrame, 4, values);
  //SET_VECTOR_ELT(dataFrame, 5, seen);
  SET_VECTOR_ELT(dataFrame, 5, codeptr);
  SET_STRING_ELT(colNames, 0, mkChar("name"));
  SET_STRING_ELT(colNames, 1, mkChar("envir"));
  SET_STRING_ELT(colNames, 2, mkChar("expr"));
  SET_STRING_ELT(colNames, 3, mkChar("eval"));
  SET_STRING_ELT(colNames, 4, mkChar("value"));
  //SET_STRING_ELT(colNames, 5, mkChar("seen"));
  SET_STRING_ELT(colNames, 5, mkChar("pointer"));

  setAttrib(dataFrame, R_RowNamesSymbol, names);
  setAttrib(dataFrame, R_NamesSymbol, colNames);

  PROTECT(class = allocVector(STRSXP, 1));
  SET_STRING_ELT(class, 0, mkChar("data.frame"));
  setAttrib(dataFrame, R_ClassSymbol, class);

  UNPROTECT(9);
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

SEXP call_function_from_dots(SEXP fun, SEXP args, SEXP envir, SEXP unforce) {
  if (!isEnvironment(envir))
    error("'envir' must be an environment");
  if (TYPEOF(args) != DOTSXP)
    error("Expected a DOTSXP, got %s", type2char(TYPEOF(args)));
  if (!isLogical(unforce) || length(unforce) != 1)
    error("Expected a scalar logical for unforce, got %s[%d]", 
          type2char(TYPEOF(unforce)), length(unforce));
  int force = LOGICAL(unforce)[0];

  SEXP call;
  PROTECT( call = allocVector(LANGSXP, length(args) + 1 )); 
  SETCAR(call, fun);
  
  SEXP in, out, name;
  int i;
  for (out = CDR(call), in = args, i = 0; 
       out != R_NilValue; 
       in=CDR(in), out=CDR(out), i++) {
    while (TYPEOF(CAR(IN)) == PROMSXP) {
        
  default: /* fall through */
    SETCAR( out, CAR(in));
    if (unforce) {
      //magic promise-unpacking here...
    }
    SET_TAG( in, TAG(out) );
    }
  }
 
  SEXP result = eval( call, envir );

  UNPROTECT(1);
  return result;
}
