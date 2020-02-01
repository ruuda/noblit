#ifndef _NOBLIT_H_
#define _NOBLIT_H_

typedef struct Database database_t;

// Destruct a database. Takes ownership of the input.
void noblit_db_free(database_t* db);

// Read the packed format into an in-memory database.
//
// Transfers ownership of the database to the caller. The caller is responsible
// for freeing the database.
database_t* noblit_db_read_packed(uint8_t const* fname, size_t fname_len);

#endif // _NOBLIT_H_
