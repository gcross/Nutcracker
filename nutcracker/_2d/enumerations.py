#@+leo-ver=5-thin
#@+node:gcross.20111110151700.1750: * @file enumerations.py
#@+<< License >>
#@+node:gcross.20111110151700.1751: ** << License >>
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20111110151700.1752: ** << Imports >>
from ..miscellaneous.enum_meta import Enum
#@-<< Imports >>

#@+others
#@+node:gcross.20111110151700.1754: ** Enumerations
#@+node:gcross.20111110151700.1755: *3* Direction
class Direction(Enum):
    right = 0
    up = 1
    left = 2
    down = 3
#@-others

#@+<< Exports >>
#@+node:gcross.20111110151700.1753: ** << Exports >>
__all__ = [
    "Direction",
]
#@-<< Exports >>
#@-leo
