where
  uint64_t db.type.name "db.type.uint64"
  string_t db.type.name "db.type.string"

assert
  -- Define an attribute named "name" of type uint64.
  name db.attribute.name "name"
  name db.attribute.type string_t
  name db.attribute.unique true
  name db.attribute.many false

  -- Define an attribute named "activation_year" of type uint64.
  level db.attribute.name "activation_year"
  level db.attribute.type uint64_t
  level db.attribute.unique false
  level db.attribute.many false

select
  level, name

┌───────┬──────┐
│ level │ name │
├───────┼──────┤
│ # 4   │ # 5  |
└───────┴──────┘

assert
  -- Create a few entities that use the new attributes.
  leon name "Leon Kowalski"
  leon activation_year 2017

  pris name "Pris Stratton"
  pris activation_year 2016

  roy name "Roy Batty"
  roy activation_year 2016

select
  leon, pris, roy

┌───────┬──────┐
│ leon  │ pris │
├───────┼──────┤
│ # 4   │ # 5  |
└───────┴──────┘
