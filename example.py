# The example from the readme, hypothetically translated to Python.

from typing import Any, Dict, List, NamedTuple, Tuple, Type, Union


class Attribute(NamedTuple):
    id_: int
    name: str
    type_: Type


class Variable(NamedTuple):
    id_: int


class Assertion:
    def __init__(self) -> None:
        self.fresh = 0
        self.datoms = []

    def variables(self, num: int) -> List[Variable]:
        begin = self.fresh
        self.fresh += num
        return [Variable(i) for i in range(begin, begin + num)]

    def assert_(
        self,
        datoms: List[Tuple[Variable, Attribute, Union[int, str, Variable]]],
    ) -> None:
        self.datoms.extend(datoms)

    def commit(self, db: 'Database') -> Tuple['Database', Dict[Variable, int]]:
        # TODO: Actually commit, return entity ids of new entities.
        return db, {}


class Selection:
    def __init__(self) -> None:
        self.fresh = 0
        self.conditions = []
        self.to_select = []

    def variables(self, num: int) -> List[Variable]:
        begin = self.fresh
        self.fresh += num
        return [Variable(i) for i in range(begin, begin + num)]

    def where(
        self,
        conditions: List[Tuple[Variable, Attribute, Union[int, str, Variable]]],
    ) -> None:
        self.conditions.extend(conditions)

    def select(self, *args: List[Variable]) -> None:
        self.to_select = args

    def execute(self, db: 'Database') -> List[list]:
        # TODO: Actually query the database.
        return []


class Database(NamedTuple):
    def get_attribute(self, name: str) -> Attribute:
        id_ = 0      # TODO: Actually resolve against database.
        type_ = Any  # TODO: Actually get from database.
        return Attribute(id_, name, type_)

    def build_assert(self) -> Assertion:
        return Assertion()

    def build_select(self) -> Selection:
        return Selection()


db = Database()

director_name = db.get_attribute("director.name")
film_title = db.get_attribute("film.title")
film_director = db.get_attribute("film.director")
film_year = db.get_attribute("film.year")

q = db.build_assert()
scott, nolan, tarantino, b, p, m, d, k = q.variables(8)

q.assert_([
    (scott, director_name, "Ridley Scott"),
    (nolan, director_name, "Christopher Nolan"),
    (tarantino, director_name, "Quentin Tarantino"),

    (b, film_title, "Blade Runner"),
    (b, film_director, scott),
    (b, film_year, 1982),

    (p, film_title, "Pulp Fiction"),
    (p, film_director, tarantino),
    (p, film_year, 1994),

    (m, film_title, "Memento"),
    (m, film_director, nolan),
    (m, film_year, 2000),

    (d, film_title, "Django Unchained"),
    (d, film_director, tarantino),
    (d, film_year, 2012),

    (k, film_title, "The Dark Knight Rises"),
    (k, film_director, nolan),
    (k, film_year, 2012),
])

db, _entity_ids = q.commit(db)

print("Films released in 2012:")
q = db.build_select()
f, title = q.variables(2)
q.where([
    (f, film_year, 2012),
    (f, film_title, title),
])
q.select(f, title)
for f_value, title_value in q.execute(db):
    print(f_value, title_value)

print("Films by Christopher Nolan:")
q = db.build_select()
f, nolan, title, year = q.variables(4)
q.where([
    (nolan, director_name, "Christopher Nolan"),
    (f, film_director, nolan),
    (f, film_title, title),
    (f, film_year, year),
])
q.select(year, title)
for year_value, title_value in q.execute(db):
    print(year_value, title_value)

print("All directors:")
q = db.build_select()
_, name = q.variables(2)
q.where([(_, director_name, name)])
q.select(name)
for name_value in q.execute(db):
    print(name_value)
