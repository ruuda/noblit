where
  a db.attribute.name name
  a db.attribute.type t
  a db.attribute.unique unique
  a db.attribute.many many
  t db.type.name type
select
  a, name, type, unique, many

┌─────┬───────────────────────┬──────────────────┬────────┬───────┐
│ a   │ name                  │ type             │ unique │ many  │
├─────┼───────────────────────┼──────────────────┼────────┼───────┤
│ # 4 │ "db.attribute.many"   │ "db.type.bool"   │ false  │ false │
│ # 1 │ "db.attribute.name"   │ "db.type.string" │ true   │ false │
│ # 2 │ "db.attribute.type"   │ "db.type.ref"    │ false  │ false │
│ # 3 │ "db.attribute.unique" │ "db.type.bool"   │ false  │ false │
│ # 5 │ "db.type.name"        │ "db.type.string" │ true   │ false │
└─────┴───────────────────────┴──────────────────┴────────┴───────┘
