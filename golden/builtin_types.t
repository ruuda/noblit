where
  t db.type.name name
select
  t, name

┌──────┬──────────────────┐
│ t    │ name             │
├──────┼──────────────────┤
│ # 7  │ "db.type.bool"   │
│ # 10 │ "db.type.bytes"  │
│ # 8  │ "db.type.ref"    │
│ # 11 │ "db.type.string" │
│ # 9  │ "db.type.uint64" │
└──────┴──────────────────┘
