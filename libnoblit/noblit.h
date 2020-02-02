#ifndef _NOBLIT_H_
#define _NOBLIT_H_

/* This file was generated from docs/reference/c.md by libnoblit/gen_header.py. */

typedef struct Noblit noblit_t;
noblit_t* noblit_db_read_packed(uint8_t const* fname, size_t fname_len);
void noblit_db_free(noblit_t* db);

#endif
