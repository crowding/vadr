#include "vadr.h"
#include "charstep.h"
/* R strings can be either Latin or UTF8 encoding. 
 * (or other things like bytes, which we'll ignore.)
 */

/* code for stepping over UTF8 */
static const unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
};

/* RReturn stepper and also pointer to buffer (in case reencoded) */
step_t get_stepper(SEXP s, const char **buf_out) {
  assert_type(s, CHARSXP);
  switch(Rf_getCharCE(s)) {
  case CE_UTF8:
    Rprintf("UTF8 encoding\n");
    *buf_out = CHAR(s);
    return &step_utf8;
  case CE_ANY:
  case CE_LATIN1:
    Rprintf("Latin encoding\n");
    *buf_out = CHAR(s);
    return &step_bytes;
  case CE_BYTES:
    error("Byte strings not supported");
  default:
    Rprintf("Re-encoding to UTF8\n");
    *buf_out = Rf_reEnc(CHAR(s), getCharCE(s), CE_UTF8, 0);
    return &step_utf8;
  }
}

const char *step_bytes(const char *s, int distance) {
  return s+distance;
}

const char *step_utf8(const char *s, int distance) {
  const char *q = s;
  for (; distance > 0; distance--) {
    q += utf8clen(*q);
  }
  for (; distance < 0; distance++) {
    q--;
    /* utf8 step backwards: skip over any 10xxxxxx to the 11xxxxxx */
    if (*q & 0x80) {
      while (((*q & 0x80) == 0x80) && (((~*q) & 0x40) == 0x40)) {
        q--;
      }
      if ((*q & 0xC0) != 0xC0) {
        error("Garbage in UTF8");
      }
    }
  }
  return q;
}

int utf8clen(char c) {
    /* This allows through 8-bit chars 10xxxxxx, which are invalid */
    if ((c & 0xc0) != 0xc0) return 1;
    return 1 + utf8_table4[c & 0x3f];
}
