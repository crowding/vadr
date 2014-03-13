#include "vadr.h"

/* Allocate blank list of dotsxps and promises */
SEXP allocate_dots(int length) {
  if (length <= 0) return R_NilValue;
  SEXP dots = PROTECT(allocList(length));
  SEXP promises = PROTECT(allocList(length));
  SEXP d = dots; SEXP p = promises; SEXP next = CDR(p);
  for (int i = 0;
       i < length;
       d = CDR(d), p = next, next = CDR(next), i++) {
    SET_TYPEOF(d, DOTSXP);
    SET_TYPEOF(p, PROMSXP);
    SET_PRENV(p, R_EmptyEnv);
    SET_PRVALUE(p, R_UnboundValue);
    SET_PRCODE(p, R_MissingArg);
    SET_PRSEEN(p, 0);
    SETCAR(d, p);
  }
  UNPROTECT(2);
  return dots;
}

SEXP _mutate_expressions(SEXP dots, SEXP new_exprs) {
  assert_type(dots, DOTSXP);
  assert_type(new_exprs, VECSXP);

  int n = length(dots);
  if (LENGTH(new_exprs) != length(dots)) {
    error("Length mismatch");
  }
    
  SEXP out = PROTECT(allocate_dots(n));

  SEXP o, d; int i;
  for(i = 0, d = dots, o = out;
      i < n;
      i++, o = CDR(o), d = CDR(d)) {
    SEXP newprom = CAR(o);
    SEXP oldprom = CAR(d);
    SET_TAG(o, TAG(d));

    if (oldprom == R_MissingArg) {
      // nothing
    } else {
      while (TYPEOF(PRCODE(oldprom)) == PROMSXP) {
        oldprom = PRCODE(oldprom);
      }
      if (PRENV(oldprom) == R_NilValue) {
        error("Can't put new expression on already-evaluated promise");
      }
      SET_PRVALUE(newprom, PRVALUE(oldprom));
      SET_PRENV(newprom, PRENV(oldprom));
    }
    SET_PRCODE(newprom, VECTOR_ELT(new_exprs, i));
  }
  DUPLICATE_ATTRIB(out, dots);
  UNPROTECT(2);
  return out;
}

SEXP _mutate_environments(SEXP dots, SEXP new_envs) {
  assert_type(dots, DOTSXP);
  assert_type(new_envs, VECSXP);

  int n = length(dots);
  if (LENGTH(new_envs) != length(dots)) {
    error("Length mismatch");
  }
    
  SEXP out = PROTECT(allocate_dots(n));

  SEXP o, d; int i;
  for(i = 0, d = dots, o = out;
      i < n;
      i++, o = CDR(o), d = CDR(d)) {
    SEXP newprom = CAR(o);
    SEXP oldprom = CAR(d);
    SET_TAG(o, TAG(d));

    if (oldprom == R_MissingArg) {
      // nothing
    } else {
      while (TYPEOF(PRCODE(oldprom)) == PROMSXP) {
        oldprom = PRCODE(oldprom);
      }
      SET_PRVALUE(newprom, R_UnboundValue);
      SET_PRCODE(newprom, PRCODE(oldprom));
    }
    SET_PRENV(newprom, VECTOR_ELT(new_envs, i));
  }
  DUPLICATE_ATTRIB(out, dots);
  UNPROTECT(2);
  return out;
}

/*
 * Local Variables:
 * eval: (previewing-mode)
 * previewing-build-command: (previewing-run-R-unit-tests)
 * End:
 */
