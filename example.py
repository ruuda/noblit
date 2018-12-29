# The example from the readme, hypothetically translated to Python.

from typing import Any, Dict, List, NamedTuple, Type, Union


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
        entity: Variable,
        attribute: Attribute,
        value: Union[int, str, Variable]
    ):
        self.datoms.append((entity, attribute, value))

    def commit(self, db: 'Database') -> Dict[Variable, int]:
        # TODO: Actually commit, return entity ids of new entities.
        return {}


class Database(NamedTuple):
    def get_attribute(self, name: str) -> Attribute:
        id_ = 0      # TODO: Actually resolve against database.
        type_ = Any  # TODO: Actually get from database.
        return Attribute(id_, name, type_)

    def build_assert(self) -> Assertion:
        return Assertion()


db = Database()
director_name = db.get_attribute("director.name")
film_title = db.get_attribute("film.title")
film_director = db.get_attribute("film.director")
film_year = db.get_attribute("film.year")

q = db.build_assert()
scott, nolan, tarantino, b, p, m, d, k = q.variables(8)

q.assert_(scott, director_name, "Ridley Scott")
q.assert_(nolan, director_name, "Christopher Nolan")
q.assert_(tarantino, director_name, "Quentin Tarantino")

q.assert_(b, film_title, "Blade Runner")
q.assert_(b, film_director, scott)
q.assert_(b, film_year, 1982)

q.assert_(p, film_title, "Pulp Fiction")
q.assert_(p, film_director, tarantino)
q.assert_(p, film_year, 1994)

q.assert_(m, film_title, "Memento")
q.assert_(m, film_director, nolan)
q.assert_(m, film_year, 2000)

q.assert_(d, film_title, "Django Unchained")
q.assert_(d, film_director, tarantino)
q.assert_(d, film_year, 2012)

q.assert_(k, film_title, "The Dark Knight Rises")
q.assert_(k, film_director, nolan)
q.assert_(k, film_year, 2012)

q.commit(db)
