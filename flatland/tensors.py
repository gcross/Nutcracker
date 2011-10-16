#@+leo-ver=5-thin
#@+node:gcross.20111009193003.1158: * @file tensors.py
#@+<< Imports >>
#@+node:gcross.20111009193003.1159: ** << Imports >>
from numpy import inner, ndarray, tensordot

from flatland.utils import crand, formContractor, mapFunctions, normalizeAndDenormalize
#@-<< Imports >>

#@+others
#@+node:gcross.20111016195932.1248: ** Classes
#@+node:gcross.20111016195932.1249: *3* MetaTensor
class MetaTensor(type):
    #@+others
    #@+node:gcross.20111009193003.1173: *4* __init__
    def __new__(cls,name,bases,data):
        if "_dimensions" in data:
            dimensions = data["_dimensions"]
            data["_dimensions"] = [name + "_dimension" for name in dimensions]
            repeated_dimensions = set()
            index = 0
            for name in dimensions:
                if name not in repeated_dimensions:
                    index_name = name + "_index"
                    if index_name in data:
                        repeated_dimensions.add(name)
                        del data[index_name]
                    else:
                        data[index_name] = index
                index += 1
        return type.__new__(cls,name,bases,data)
    #@-others
#@+node:gcross.20111009193003.1162: *3* Tensor
class Tensor(metaclass=MetaTensor):
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
            fill = None
            for given_dimension_name in keywords.keys():
                if given_dimension_name == "randomize":
                    randomize = keywords["randomize"]
                elif given_dimension_name == "fill":
                    fill = keywords["fill"]
                elif given_dimension_name not in self._dimensions:
                    if given_dimension_name + "_dimension" in self._dimensions:
                        raise ValueError("you needed to type '{}_dimension' rather than '{}' in the list of keyword arguments to supply the {} dimension".format(*(given_dimension_name,)*3))
                    else:
                        raise ValueError("{} is not a recognized dimension for this tensor".format(given_dimension_name))
            shape = []
            for name in self._dimensions:
                try:
                    dimension = keywords[name]
                    shape.append(dimension)
                    setattr(self,name,dimension)
                except KeyError:
                    raise ValueError("missing a value for dimension {}".format(name))
            if randomize and fill:
                raise ValueError("you asked to fill the tensor both with random data *and* a given constant, which are contradictory requests")
            if randomize:
                self.data = crand(*shape)
            else:
                self.data = ndarray(shape)
            if fill:
                self.data[...] = fill
    #@+node:gcross.20111009193003.5259: *4* trivial
    @classmethod
    def trivial(cls):
        keywords = {name: 1 for name in cls._dimensions}
        keywords["fill"] = 1
        return cls(**keywords)
    #@-others
#@+node:gcross.20111009193003.1161: *3* Tensors
#@+others
#@+node:gcross.20111009193003.5252: *4* StateCenterSite
class StateCenterSite(Tensor):
    _dimensions = ["physical","rightward","upward","leftward","downward"]
    #@+others
    #@+node:gcross.20111014113710.1232: *5* bandwidthDimension
    def bandwidthDimension(self,direction):
        return self.data.shape[1+direction]
    #@+node:gcross.20111014113710.1233: *5* bandwidthDimensions
    def bandwidthDimensions(self):
        return self.data.shape[1:]
    #@-others
#@+node:gcross.20111009193003.5243: *4* StateCornerBoundary
class StateCornerBoundary(Tensor):
    _dimensions = ["clockwise","counterclockwise"]
    #@+others
    #@-others
#@+node:gcross.20111009193003.5232: *4* StateCornerSite
class StateCornerSite(Tensor):
    _dimensions = ["physical","clockwise","counterclockwise"]
    #@+others
    #@+node:gcross.20111013080525.1207: *5* absorbSideSiteAtClockwise
    def absorbSideSiteAtClockwise(self,side):
        return StateCornerSite(
             tensordot(self.data,side.data,(1,2))
            .transpose(0,2,3,1,4)
            .reshape(
                self.physical_dimension*side.physical_dimension,
                side.clockwise_dimension,
                self.counterclockwise_dimension*side.inward_dimension,
             )
        )
    #@+node:gcross.20111013080525.1203: *5* absorbSideSiteAtCounterClockwise
    def absorbSideSiteAtCounterClockwise(self,side):
        return StateCornerSite(
             tensordot(self.data,side.data,(2,1))
            .transpose(0,2,1,4,3)
            .reshape(
                self.physical_dimension*side.physical_dimension,
                self.clockwise_dimension*side.inward_dimension,
                side.counterclockwise_dimension,
             )
        )
    #@+node:gcross.20111009193003.5237: *5* formBoundary
    def formBoundary(self):
        return StateCornerBoundary(
             tensordot(self.data,self.data.conj(),(0,0))
            .transpose(0,2,1,3)
            .reshape(
                self.clockwise_dimension*self.clockwise_dimension,
                self.counterclockwise_dimension*self.counterclockwise_dimension,
             )
        )
    #@-others
#@+node:gcross.20111009193003.1166: *4* StateSideBoundary
class StateSideBoundary(Tensor):
    _dimensions = ["clockwise","counterclockwise","inward","inward"]
    #@+others
    #@+node:gcross.20111009193003.5244: *5* absorbCounterClockwiseCornerBoundary
    def absorbCounterClockwiseCornerBoundary(self,corner):
        return StateSideBoundary(
             tensordot(self.data,corner.data,(1,0))
            .transpose(0,3,1,2)
        )
    #@+node:gcross.20111009193003.5262: *5* absorbCounterClockwiseSideBoundary
    def absorbCounterClockwiseSideBoundary(self,side):
        return StateSideBoundary(
             tensordot(self.data,side.data,(1,0))
            .transpose(0,3,1,4,2,5)
            .reshape(
                self.clockwise_dimension,
                side.counterclockwise_dimension,
                self.inward_dimension*side.inward_dimension,
                self.inward_dimension*side.inward_dimension,
             )
        )
    #@-others
#@+node:gcross.20111009193003.1164: *4* StateSideSite
class StateSideSite(Tensor):
    _dimensions = ["physical","clockwise","counterclockwise","inward"]
    #@+others
    #@+node:gcross.20111013080525.1234: *5* absorbCenterSite
    def absorbCenterSite(self,center,direction):
        return StateSideSite(self.absorbCenterSite.contractors[direction](self.data,center.data))

    absorbCenterSite.contractors = [
        formContractor(
            ['S','C'],
            [
                (('S',3),('C',1+i)),
            ],
            [
                [('S',0),('C',0)],
                [('S',1),('C',1+(i-1)%4)],
                [('S',2),('C',1+(i+1)%4)],
                [('C',1+(i+2)%4),]
            ]
        ) for i in range(4)
    ]
    #@+node:gcross.20111009193003.1165: *5* formBoundary
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
        )
    #@+node:gcross.20111016195932.1252: *5* normalizeSelfAndDenormalizeCenter
    def normalizeSelfAndDenormalizeCenter(self,center,direction):
        return mapFunctions(
            (StateSideSite,StateCenterSite),
            normalizeAndDenormalize(self.data,self.inward_index,center.data,1+direction)
        )
    #@-others
#@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111009193003.1171: ** << Exports >>
__all__ = [
    "StateCenterSite",
    "StateCornerBoundary",
    "StateCornerSite",
    "StateSideBoundary",
    "StateSideSite",
]
#@-<< Exports >>
#@-leo
