#include "vadr.h"

SEXP emptypromise() {
  SEXP out = PROTECT(allocSExp(PROMSXP));
  SET_PRCODE(out, R_MissingArg);
  SET_PRENV(out, R_EmptyEnv);
  SET_PRVALUE(out, R_UnboundValue);
  UNPROTECT(1);
  return out;
}

/* because this is not exposed in Rinternals.h for some reason */
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

/* If not a promise, wrap in a promise. */
SEXP make_into_promise(SEXP in) {
  if (TYPEOF(in) == PROMSXP) {
    while (TYPEOF(PREXPR(in)) == PROMSXP) {
      in = PREXPR(in);
    }
    return in;
  } else {
    /* wrap in a promise */
    SEXP out = PROTECT(allocSExp(PROMSXP));
    SET_PRENV(out, R_EmptyEnv);
    SET_PRVALUE(out, in);
    SET_PRCODE(out, in);
    UNPROTECT(1);
    return out;
  }
}

/* Extract named variables from an environment into a dotslist */
SEXP _env_to_dots(SEXP envir, SEXP names) {
  assert_type(envir, ENVSXP);
  assert_type(names, STRSXP);
  int length = LENGTH(names);
  SEXP out;
  if (length >= 1) {
    SEXP tail;
    for (int i = 0; i < length; i++) {
      if (i == 0) {
        out = PROTECT(allocSExp(DOTSXP));
        tail = out;
      } else {
        SEXP new = allocSExp(DOTSXP);
        SETCDR(tail, new);
        tail = new;
      }
      SEXP sym = install(CHAR(STRING_ELT(names, i)));
      SEXP found = Rf_findVar(sym, envir);
      Rprintf("got %s\n", CHAR(PRINTNAME(sym)));
      if (found == R_UnboundValue) {
        error("Variable `%s` was not found.",
              CHAR(PRINTNAME(sym)));
      }
      if (sym == R_DotsSymbol) {
        assert_type(found, DOTSXP);
        /* copy all the dotslist */
        while (1) {
          SEXP tag = TAG(found);
          if (tag != R_NilValue) {
            Rprintf("got ..( %s )\n", CHAR(PRINTNAME(tag)));
          } else {
            Rprintf("got ..(unnamed argument)\n");
          }
          SET_TAG(tail, tag);
          SETCAR(tail, CAR(found));
          Rprintf("setting a dots %s\n", type2char(TYPEOF(CAR(tail))));
          found = CDR(found);
          if (found != R_NilValue) {
            SEXP new = allocSExp(DOTSXP);
            SETCDR(tail, new);
            tail = new;
          } else {
            break;
          }
        }
      } else {
        SET_TAG(tail, sym);
        SEXP made = make_into_promise(found);
        SETCAR(tail, made);
        Rprintf("setting a %s\n", type2char(TYPEOF(CAR(tail))));
      }
    }
  } else {
    out = PROTECT(allocVector(VECSXP, 0));
  }
  setAttrib(out, R_ClassSymbol, ScalarString(mkChar("...")));
  {
    SEXP what = out;
    int where = 0;
    for (;
         what != R_NilValue;
         what = CDR(what), where += 1) {
      Rprintf("element %d is a %s\n", where, type2char(TYPEOF(CAR(what))));
    }
  }
  UNPROTECT(1);
  return out;
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
