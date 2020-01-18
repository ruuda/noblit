where
  t db.type.name name
select
  t, name

> ┌──────┬──────────────────┐
> │ t    │ name             │
> ├──────┼──────────────────┤
> │ # 6  │ "db.type.bool"   │
> │ # 7  │ "db.type.ref"    │
> │ # 8  │ "db.type.uint64" │
> │ # 9  │ "db.type.bytes"  │
> │ # 10 │ "db.type.string" │
> └──────┴──────────────────┘
