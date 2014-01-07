#include "vadr.h"

SEXP _getpromise_in(SEXP envir, SEXP names) {
  assert_type(envir, VECSXP);
  assert_type(names, VECSXP);
  SEXP tags = getAttrib(names, R_NamesSymbol);
  int len = length(names);
  
  SEXP output = PROTECT(allocList(len));
  SEXP output_iter = output;
    for (int i = 0; i < len; i++, output_iter = CDR (output_iter)) {
    SET_TYPEOF(output_iter, DOTSXP);
    if ((tags != R_NilValue) && (STRING_ELT(tags, i) != R_BlankString)) {
      SET_TAG(output_iter, install(CHAR(STRING_ELT(tags, i))));
    }
    SEXP name = VECTOR_ELT(names, i);
    assert_type(name, SYMSXP);
    SEXP promise = Rf_findVar(name, envir);
    assert_type(promise, PROMSXP);
    SETCAR(output_iter, promise);
  }
  setAttrib(output, R_ClassSymbol, ScalarString(mkChar("...")));
  UNPROTECT(1);
  return(output);
}

SEXP _arg_env(SEXP envir, SEXP name) {
  assert_type(envir, ENVSXP);
  assert_type(name, SYMSXP);
  SEXP promise = Rf_findVar(name, envir);
  assert_type(promise, PROMSXP);
  SEXP out = PRENV(out);
  if (out == R_NilValue) {
    error("Promise has already been evaluated (no environment attached)");
  }
  return out;
}

SEXP _arg_expr(SEXP envir, SEXP name) {
  assert_type(envir, ENVSXP);
  assert_type(name, SYMSXP);
  SEXP promise = Rf_findVar(name, envir);
  assert_type(promise, PROMSXP);
  SEXP out = PREXPR(out);
  return out;
}

/*
 * Local Variables:
 * eval: (previewing-mode)
 * previewing-build-command: (previewing-run-R-unit-tests)
 * End:
 */
