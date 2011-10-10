#@+leo-ver=5-thin
#@+node:gcross.20111009193003.1158: * @file tensors.py
#@+<< Imports >>
#@+node:gcross.20111009193003.1159: ** << Imports >>
from numpy import inner, ndarray, tensordot

from flatland.utils import crand, formContractor
#@-<< Imports >>

#@+others
#@+node:gcross.20111009193003.1172: ** Functions
#@+node:gcross.20111009193003.1173: *3* addDimensionSuffixTo
def addDimensionSuffixTo(*args):
    return [name + "_dimension" for name in args]
#@+node:gcross.20111009193003.1161: ** Tensors
#@+others
#@+node:gcross.20111009193003.1162: *3* [base class Tensor]
class Tensor(object):
    #@+others
    #@+node:gcross.20111009193003.1163: *4* __init__
    def __init__(self,*args,**keywords):
        if not ((len(args) > 0) ^ (len(keywords) > 0)):
            raise ValueError("constructor must be given either a reference to the data or the dimensions of the tensor")
        if len(args) == 1:
            self.data = args[0]
            if(self.data.ndim != len(self._dimensions)):
                raise ValueError("constructor was given a reference to a tensor of rank {}, when a tensor of rank {} was required".format(self.data.ndim,len(self._dimensions)))
            for (name,dimension) in zip(self._dimensions,self.data.shape):
                setattr(self,name,dimension)
        else:
            randomize = False
            for given_dimension_name in keywords.keys():
                if given_dimension_name == "randomize":
                    randomize = True
                    continue
                if given_dimension_name not in self._dimensions:
                    raise ValueError("{} is not a recognized dimension for this tensor".format(given_dimension_name))
            shape = []
            for name in self._dimensions:
                try:
                    dimension = keywords[name]
                    shape.append(dimension)
                    setattr(self,name,dimension)
                except KeyError:
                    raise ValueError("missing a value for dimension {}".format(name))
            if randomize:
                self.data = crand(*shape)
            else:
                self.data = ndarray(shape)
    #@-others
#@+node:gcross.20111009193003.1166: *3* StateSideBoundary
class StateSideBoundary(Tensor):
    _dimensions = addDimensionSuffixTo("clockwise","counterclockwise","inward","inward")
    #@+others
    #@-others
#@+node:gcross.20111009193003.1164: *3* StateSideSite
class StateSideSite(Tensor):
    _dimensions = addDimensionSuffixTo("physical","clockwise","counterclockwise","inward")
    #@+others
    #@+node:gcross.20111009193003.1165: *4* formBoundary
    def formBoundary(self):
        return StateSideBoundary(
             tensordot(self.data,self.data.conj(),(0,0))
            .transpose(0,3,1,4,2,5)
            .reshape(
                self.clockwise_dimension*self.clockwise_dimension,
                self.counterclockwise_dimension*self.counterclockwise_dimension,
                self.inward_dimension,
                self.inward_dimension
             )
            .copy()
        )
    #@-others
#@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111009193003.1171: ** << Exports >>
__all__ = [
    "StateSideBoundary",
    "StateSideSite",
]
#@-<< Exports >>
#@-leo
