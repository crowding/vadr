#include "vadr.h"

/* Assert that some object is a type. */
void assert_type(SEXP x, SEXPTYPE type) {
  if (TYPEOF(x) != type) {
    error("Expected %s, got %s", type2char(type), type2char(TYPEOF(x)));
  }
}

/* As above, with extra words about what thing is to be an expected type */
void assert_type3(SEXP x, SEXPTYPE type, const char *what) {
  if (TYPEOF(x) != type) {
    error("Expected %s in %s, got %s",
          type2char(type), what, type2char(TYPEOF(x)));
  }
}
