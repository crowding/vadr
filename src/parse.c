#include <string.h>
#include <ctype.h>
#include "vadr.h"
#include "charstep.h"

/* An R extension function that locates substrings of the form ".(expr)"
 * where "expr" is balanced according to R syntax.
 * Should correctly handle braces () [] {},
 * %operators%, and "strings" '(with \'escapes\' and parens)'
 * Example: given a string like
 *                 "this .( c('(', '')[p] )is.( c(')', 'n\\'t')[p] ) in parens"
 * should locate these:  ^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^
 */

SEXP _find_subst_expressions_list(SEXP, SEXP, SEXP);
SEXP find_subst_expressions(SEXP, SEXP, SEXP);

const char *block_search(const char *, const char *, const char *,
                         const char **, const char **,
                         const char **, const char **,
                         step_t step);
const char *forward_balanced(const char *str, step_t);
const char *forward_word_or_char(const char *, step_t);
const char *paren_match (const char *, step_t);
const char *quoted_match(const char *open, step_t);
const char *comment_match(const char *open, step_t);

#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define MIN3(x, y, z) (MIN(x,(MIN(y,z))))
#define MAX3(x, y, z) (MAX(x,(MAX(y,z))))

/* Find substrings matching .(expr), where expr is a balanced expression
 * (following R rules for matching [], (), {}, '', "", ``, %%, \, and comments.)
 * Returns a _list_ of _character arrays_ one array per each input string.
 * Each character array alternates between "outside quote" and "inside quote."
 */
SEXP _find_subst_expressions_list(SEXP strs, SEXP begin, SEXP end) {
  int ns, nb, ne;
  assert_type(strs, STRSXP);
  assert_type(begin, STRSXP);
  assert_type(end, STRSXP);
  ns = LENGTH(strs);
  nb = LENGTH(begin);
  ne = LENGTH(end);

  int nout;
  if (MIN3(ns, nb, ne) == 0) {
    nout = 0;
  } else {
    nout = MAX3(ns, nb, ne);
    if ((nout % ns > 0)||(nout % nb > 0)||(nout % ne > 0)) {
      warning("longer object length is not a multiple"
              " of shorter object length");
    }
  }

  SEXP out = PROTECT(allocVector(VECSXP, nout));
  if (ns == nout)
    setAttrib(out, R_NamesSymbol, getAttrib(strs, R_NamesSymbol));
  for (int i = 0; i < nout; i++) {
    SET_VECTOR_ELT(out, i,
                   find_subst_expressions(STRING_ELT(strs, i%ns),
                                          STRING_ELT(begin, i%nb),
                                          STRING_ELT(end, i%ne)));
  }

  UNPROTECT(1);
  return out;
}

/* Operates on three CHARSXP (internal) arguments. Returns STRSXP vector.
 * Returned strings alternate between "outside" and "inside" blocks.
 * E.g. "Four .(score) and seven .(years)"
 * returns c("Four ", "score", " and seven ", "years")
 */
SEXP find_subst_expressions(SEXP str, SEXP begin_delim, SEXP end_delim) {
  const char *s, *b, *e;
  const char *p, *start;
  const char *before, *begin, *end, *after;
  cetype_t enc;
  step_t step;
  int nBlocks, i;
  SEXP out;

  assert_type(str, CHARSXP);
  assert_type(begin_delim, CHARSXP);
  assert_type(end_delim, CHARSXP);

  step = get_stepper(str, &s, &enc);

  if (getCharCE(begin_delim) != enc) {
    b = Rf_reEnc(CHAR(begin_delim), getCharCE(begin_delim), enc, 0);
  } else {
    b = CHAR(begin_delim);
  }

  if (getCharCE(end_delim) != enc) {
    e = Rf_reEnc(CHAR(end_delim), getCharCE(end_delim), enc, 0);
  } else {
    e = CHAR(end_delim);
  }

  /* Scan once to count how many blocks we need, then scan again (ugh) */
  nBlocks = 0;
  p = s;
  while(p && *p) {
    if ( (p = block_search(p, b, e, NULL, NULL, NULL, NULL, step)) ) {
      nBlocks++;
    }
  }

  out = PROTECT(allocVector(STRSXP, 2*nBlocks+1));
  start = s;
  p = s;
  for (i = 0; i < nBlocks; i++) {
    p = block_search(p, b, e,
                     &before, &begin, &end, &after,
                     step);
    /* extract leading unescaped block, then escaped block */
    if (p) {
      SET_STRING_ELT(out, 2*i, mkCharLenCE(start, before-start, enc));
      SET_STRING_ELT(out, 2*i+1, mkCharLenCE(begin, end-begin, enc));
      start = after;
    }
  }
  /* then the rest of the string. */
  SET_STRING_ELT(out, 2*i, mkCharCE(start, enc));

  UNPROTECT(1);
  return out;
}

/* A simple parser.
 * Search for substrings of form .(expr),
 * where expr is property paren- and quote-balanced.
 * The start and end of expr and block are returned via out parameter.
 * Return value is end of search (same as *after_out) or NULL if nothing found.
 */
const char *block_search(const char *str, // a string to search
                         const char *begin_mark, // begin delimiter e.g. ".("
                         const char *end_mark, // ending delimiter e.g. ")"
                         const char **before_out, // returns location of ".("
                         const char **begin_out, // returns beginning of expr
                         const char **end_out, // returns end of expr
                         const char **after_out, // returns after end mark.
                         step_t step //function for stepping over string
                         ) {
  int n = strlen(begin_mark);
  int m = strlen(end_mark);
  for (const char *s = str; *s; s=(*step)(s, 1)) {
    if (!strncmp(s, begin_mark, n)) {
      const char *ss = s+n;
      while (ss = forward_balanced(ss, step)) {
        if (!strncmp(ss, end_mark, m)) {
          if (before_out) *before_out = s;
          if (begin_out) *begin_out = s+n;
          if (end_out) *end_out = ss;
          if (after_out) *after_out = ss+m;
          return ss+m;
        }
      }
    }
  }
  return NULL;
}

/* Go forward one symbol, balanced subexpression, or other char. 
 * Return NULL if imbalanced. */
const char *forward_balanced(const char *str, step_t step) {
  switch (*str) {
  case 0:
    return NULL;
  case '(':
  case '{':
  case '[':
    return paren_match(str, step);
  case ')':
  case '}':
  case ']':
    return NULL;
  case '"':
  case '\'':
  case '%':
  case '`':
    return quoted_match(str, step);
  case '#':
    return comment_match(str, step);
  default:
    return forward_word_or_char(str, step);
  }
}

int isstartword(char c) {return (isalpha(c) || c == '.');}
int iswordchar(char c) {return (isalnum(c) || c == '.' || c == '_');}

const char *forward_word_or_char(const char *str, step_t step) {
  if (*str && isstartword(*str)) {
    for (str = (*step)(str, 1);
         *str && iswordchar(*str);
         str = (*step)(str, 1));
  } else {
    str = (*step)(str, 1);
  }
  return str;
}

/* Takes a pointer to an opening parenthetical.
 * Returns a pointer AFTER the matching delimiter, or NULL if not found.
 */
const char *paren_match (const char *open, step_t step) {
  const char *s = (*step)(open, 1);
  while (*s) {
    switch (*s) {
    case '#':
      s = comment_match(s, step);
      if (s) {
        break;
      } else return NULL;
    case '(':
    case '{':
    case '[':
      s = paren_match(s, step);
      if (s) {
        break;
      } else return NULL;
    case ']':
      if (*open == '[') return (*step)(s,1); else return NULL;
    case '}':
      if (*open == '{') return (*step)(s,1); else return NULL;
    case ')':
      if (*open == '(') {
        return (*step)(s,1);
      } else return NULL;
    case '"':
    case '\'':
    case '%':
    case '`':
      s = quoted_match(s, step);
      if (s) {
        break;
      } else return NULL;
    default:
      s = (*step)(s, 1);
    }

  }
  return NULL;
}

/* Search for a closing quote, given pointer to the matching opening
 * quote. Return pointer AFTER closing quote or NULL if no match found.
 */
const char *quoted_match(const char *open, step_t step) {
  const char *s = (*step)(open, 1);
  while (*s) {
    switch(*s) {
    case '\\':
      if (*open != '%') { //infix operators don't backslash escape
        s = (*step)(s, 1);  //skip char
        if (!*s) return NULL;
      }
      s = (*step)(s, 1);
      break;
    case '\'':
    case '"' :
    case '%' :
    case '`':
      if (*s == *open) return (*step)(s,1);
      else s = (*step)(s, 1);
      break;
    default:
      s = (*step)(s, 1);
    }
  }
  return NULL; // no match found
}

/* Search for end of a comment. Return pointer after comment, or
 * NULL if string ends first.
 */
const char *comment_match(const char *open, step_t step) {
  const char *s = (*step)(open, 1);
  while (*s) {
    switch (*s) {
    case 0:
      return NULL;
    case '\n':
      return (*step)(s,1);
    case '\r':
      return (*step)(s,1);
    default:
      s = (*step)(s, 1);
    }
  }
  return NULL;
}

/*
 * Local Variables:
 * eval: (previewing-mode)
 * previewing-build-command: (previewing-run-R-unit-tests)
 * End:
 */
