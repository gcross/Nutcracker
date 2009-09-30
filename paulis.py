#@+leo-ver=4-thin
#@+node:cog.20080522002115.2:@thin paulis.py
from numpy import array, complex128

__all__ = ["I","X","Y","Z"]

Z = array([1,0,0,-1],complex128).reshape(2,2)
Y = array([0,1j,-1j, 0],complex128).reshape(2,2)
X = array([0,1,1, 0],complex128).reshape(2,2)
I = array([1,0,0, 1],complex128).reshape(2,2)
#@-node:cog.20080522002115.2:@thin paulis.py
#@-leo
