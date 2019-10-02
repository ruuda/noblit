#define is_attribute(A, NAME, TYPE, UNIQUE, MANY) \
  A db.attribute.name NAME     \
  A db.attribute.type TYPE     \
  A db.attribute.unique UNIQUE \
  A db.attribute.many MANY

where
  uint64 db.type.name "db.type.uint64"
  string db.type.name "db.type.string"
assert
  is_attribute(level, "level", uint64, false, false)
  is_attribute(name,  "name",  string, true,  false)
