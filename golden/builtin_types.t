where
  t db.type.name name
select
  t, name

┌──────┬──────────────────┐
│ t    │ name             │
├──────┼──────────────────┤
│ # 6  │ "db.type.bool"   │
│ # 9  │ "db.type.bytes"  │
│ # 7  │ "db.type.ref"    │
│ # 10 │ "db.type.string" │
│ # 8  │ "db.type.uint64" │
└──────┴──────────────────┘
