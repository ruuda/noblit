define(is_attribute,
  $1 db.attribute.name $2
  $1 db.attribute.type $3
  $1 db.attribute.unique $4
  $1 db.attribute.many $5)

where
  uint64 db.type.name "db.type.uint64"
  string db.type.name "db.type.string"
assert
  is_attribute(level, "level", uint64, false, false)
  is_attribute(name,  "name",  string, true,  false)
