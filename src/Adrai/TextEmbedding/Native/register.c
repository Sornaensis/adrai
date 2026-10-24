/* Register with the same statically linked SQLite used by direct-sqlite. */
#include "sqlite-vec.h"
int adrai_register_sqlite_vec(void) {
  return sqlite3_auto_extension((void (*)(void)) sqlite3_vec_init);
}
