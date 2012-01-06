from ..miscellaneous.enum_meta import Enum

class Direction(Enum):
    left = "left"
    right = "right"
class Normalization(Direction):
    middle = "middle"
    none = "none"

__all__ = [
    "Direction",
    "Normalization",
]
