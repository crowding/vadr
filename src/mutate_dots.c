#include "vadr.h"

/* Allocate blank list of dotsxps and promises */
SEXP allocate_dots(int length) {
  if (length <= 0) return R_NilValue;
  SEXP dots = PROTECT(allocList(length));
  SEXP promises = PROTECT(allocList(length));
  SEXP d = dots; SEXP p = promises; SEXP next = CDR(p);
  for (int i = 0; i < length;
       d = CDR(d), p = next, next=CDR(next)) {
    SET_TYPEOF(d, DOTSXP);
    SET_TYPEOF(p, PROMSXP);
    SET_PRENV(p, R_NilValue);
    SET_PRVALUE(p, R_UnboundValue);
    SET_PRCODE(p, R_NilValue);
    SET_PRSEEN(p, 0);
    SETCAR(d, p);
  }
  UNPROTECT(2);
  return d;
}

SEXP _mutate_expressions(SEXP dots, SEXP new_exprs) {
  assert_type(dots, DOTSXP);
  assert_type(new_exprs, VECSXP);

  int n = length(dots);
  if (LENGTH(new_exprs) != length(dots)) {
    error("Length mismatch");
  }
    
  /* Hackish. These get turned into dots and promises respectively... */
  SEXP out = PROTECT(allocList(length(dots)));
  SEXP promises = PROTECT(allocList(length(dots)));

  SEXP next_prom, p, o; int i;
  next_prom = promises;
  for(i = 0, p = dots, o = out;
      i < n;
      i++, p = CDR(p), o = CDR(o)) {
    SEXP newprom = next_prom;
    SEXP oldprom = CAR(p);
    while (TYPEOF(PRCODE(oldprom)) == PROMSXP) {
      oldprom = PRCODE(oldprom);
    }
    if (PRENV(oldprom) == R_NilValue) {
      error("Can't put new expression on already-evaluated promise");
    }
    next_prom = CDR(next_prom);
    
    SET_TYPEOF(o, DOTSXP);
    SET_TAG(o, TAG(p));
    SET_TYPEOF(newprom, PROMSXP);
    SET_PRCODE(newprom, VECTOR_ELT(new_exprs, i));
    SET_PRVALUE(newprom, PRVALUE(oldprom));
    SET_PRENV(newprom, PRENV(oldprom));
    SETCAR(o, newprom);
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
    
  /* Hackish. These get turned into dots and promises respectively... */
  SEXP out = PROTECT(allocList(LENGTH(new_envs)));
  SEXP promises = PROTECT(allocList(LENGTH(new_envs)));

  SEXP next_prom, p, o; int i;
  next_prom = promises;
  for(i = 0, p = dots, o = out;
      i < n;
      i++, p = CDR(p), o = CDR(o)) {
    SEXP newprom = next_prom;
    SEXP oldprom = CAR(p);
    next_prom = CDR(next_prom);

    while (TYPEOF(PRCODE(oldprom)) == PROMSXP) {
      oldprom = PRCODE(oldprom);
    }
    
    SET_TYPEOF(o, DOTSXP);
    SET_TAG(o, TAG(p));
    SET_TYPEOF(newprom, PROMSXP);
    assert_type(VECTOR_ELT(new_envs, i), ENVSXP);
    SET_PRENV(newprom, VECTOR_ELT(new_envs, i));
    SET_PRVALUE(newprom, R_UnboundValue);
    SET_PRCODE(newprom, PRCODE(oldprom));
    SET_PRSEEN(newprom, 0);
    SETCAR(o, newprom);
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
