# https://www.python.org/dev/peps/pep-0526/

primes: List[int] = []

captain: str  # Note: no initial value!


some_number: int           # variable without initial value
some_list: List[int] = []  # variable with initial value

sane_world: bool
if 2+2 == 4:
    sane_world = True
else:
    sane_world = False

# Tuple packing with variable annotation syntax
t: Tuple[int, ...] = (1, 2, 3)

# Tuple unpacking with variable annotation syntax
header: str
kind: int
body: Optional[List[str]]
header, kind, body = message


class BasicStarship:
    captain: str = 'Picard'               # instance variable with default
    damage: int                           # instance variable without default
    stats: ClassVar[Dict[str, int]] = {}  # class variable

class Starship:
    captain: str = 'Picard'
    damage: int
    stats: ClassVar[Dict[str, int]] = {}

    def __init__(self, damage: int, captain: str = None):
        self.damage = damage
        if captain:
            self.captain = captain  # Else keep the default

    def hit(self):
        Starship.stats['hits'] = Starship.stats.get('hits', 0) + 1

enterprise_d = Starship(3000)
enterprise_d.stats = {} # Flagged as error by a type checker
Starship.stats = {} # This is OK


class Cls:
    pass

c = Cls()
c.x: int = 0  # Annotates c.x with int.
c.y: int      # Annotates c.y with int.

d = {}
d['a']: int = 0  # Annotates d['a'] with int.
d['b']: int      # Annotates d['b'] with int.


qualified: typing.Any = 'anything'
