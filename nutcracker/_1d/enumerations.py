#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1654: * @file enumerations.py
#@+<< Imports >>
#@+node:gcross.20111109104457.1656: ** << Imports >>
from ..miscellaneous.enum_meta import Enum
#@-<< Imports >>

#@+others
#@+node:gcross.20111108100704.1380: ** Enumerations
#@+node:gcross.20111108100704.1382: *3* Direction
class Direction(Enum):
    left = "left"
    right = "right"
#@+node:gcross.20111108100704.1381: *3* Normalization
class Normalization(Direction):
    middle = "middle"
    none = "none"
#@-others

#@+<< Exports >>
#@+node:gcross.20111109104457.1657: ** << Exports >>
__all__ = [
    "Direction",
    "Normalization",
]
#@-<< Exports >>
#@-leo
