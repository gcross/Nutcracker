from .miscellaneous.enum_meta import Enum

class Sparsity(Enum):
    dense = "dense"
    sparse = "sparse"

__all__ = [
    "Sparsity",
]
