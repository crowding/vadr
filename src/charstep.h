/* One needs different methods for stepping to the next character depending on encoding. */

typedef const char *(*step_t)(const char *, int);

step_t get_stepper(SEXP, const char **, cetype_t *);
const char *step_utf8(const char *, int);
const char *step_bytes(const char *, int);
int utf8clen(char c);
