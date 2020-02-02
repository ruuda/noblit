#ifndef _NOBLIT_H_
#define _NOBLIT_H_

// See docs/reference.md for the API documentation.

typedef struct Noblit noblit_t;
void noblit_db_free(noblit_t* db);

noblit_t* noblit_db_read_packed(uint8_t const* fname, size_t fname_len);

#endif
