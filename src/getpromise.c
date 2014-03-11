#include "vadr.h"

SEXP emptypromise() {
  SEXP out = PROTECT(allocSExp(PROMSXP));
  SET_PRCODE(out, R_MissingArg);
  SET_PRENV(out, R_EmptyEnv);
  SET_PRVALUE(out, R_UnboundValue);
  UNPROTECT(1);
  return out;
}


SEXP do_ddfindVar(SEXP symbol, SEXP envir) {
  int i;
  SEXP vl;

  vl = findVar(R_DotsSymbol, envir);
  i = DDVAL(symbol);
  if (vl != R_UnboundValue) {
    if (length(vl) >= i) {
      vl = nthcdr(vl, i - 1);
      return(CAR(vl));
    }
    else
      error("the ... list does not contain %d elements", i);
  }
  else error("..%d used in an incorrect context, no ... to look in", i);

  return R_NilValue;
}

SEXP do_findPromise(SEXP name, SEXP envir) {
  assert_type(name, SYMSXP);
  assert_type(envir, ENVSXP);
  SEXP promise;
  if (DDVAL(name)) {
    promise = do_ddfindVar(name, envir);
  } else {
    promise = Rf_findVar(name, envir);
  }
  if (promise == R_UnboundValue) {
    error("Variable `%s` was not found.",
          CHAR(PRINTNAME(name)));
  }
  if (promise == R_MissingArg) {
    promise = emptypromise();
  }
  if (TYPEOF(promise) != PROMSXP) {
    error("Variable `%s` was not bound to a promise",
          CHAR(PRINTNAME(name)));
  }
  return promise;
}

SEXP _getpromise_in(SEXP envirs, SEXP names, SEXP tags) {
  assert_type(envirs, VECSXP);
  assert_type(names, VECSXP);
  assert_type(tags, STRSXP);
  int len = length(names);
  
  SEXP output = PROTECT(allocList(len));
  SEXP output_iter = output;
  for (int i = 0; i < len; i++, output_iter = CDR (output_iter)) {
    SET_TYPEOF(output_iter, DOTSXP);
    if ((tags != R_NilValue) && (STRING_ELT(tags, i) != R_BlankString)) {
      SET_TAG(output_iter, install(CHAR(STRING_ELT(tags, i))));
    }
    SEXP promise = do_findPromise(VECTOR_ELT(names, i),
                                  VECTOR_ELT(envirs, i));

    while (TYPEOF(PREXPR(promise)) == PROMSXP) {
      promise = PREXPR(promise);
    }
    SETCAR(output_iter, promise);
  }
  setAttrib(output, R_ClassSymbol, ScalarString(mkChar("...")));
  UNPROTECT(1);
  return(output);
}

SEXP _arg_env(SEXP envir, SEXP name) {
  assert_type(envir, ENVSXP);
  assert_type(name, SYMSXP);
  SEXP promise = do_findPromise(name, envir);
  while (TYPEOF(PREXPR(promise)) == PROMSXP) {
    promise = PREXPR(promise);
  }
  SEXP out = PRENV(promise);
  if (out == R_NilValue) {
    error("Promise bound to `%s` has already been evaluated"
          " (no environment attached)", CHAR(PRINTNAME(name))
          );
  }
  return out;
}

SEXP _arg_expr(SEXP envir, SEXP name) {
  assert_type(envir, ENVSXP);
  assert_type(name, SYMSXP);
  SEXP promise = do_findPromise(name, envir);
  while (TYPEOF(PREXPR(promise)) == PROMSXP) {
    promise = PREXPR(promise); 
  }
  SEXP out = PREXPR(promise);
  return out;
}

/*
 * Local Variables:
 * eval: (previewing-mode)
 * previewing-build-command: (previewing-run-R-unit-tests)
 * End:
 */
