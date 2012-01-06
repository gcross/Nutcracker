# Imports {{{
from collections import namedtuple
from numpy import array, complex128, ndarray, ones, zeros
from numpy.linalg import norm
from numpy.random import randint

from .enumerations import Direction, Normalization
from .. import core
from ..tensors import *
from ..utils import crand, mapFunctions, normalize
# }}}

# Classes {{{

#   Base Classes {{{

class OperatorSiteTensor(SiteTensor):
    dimension_names = ["left","right","physical_conjugate","physical"]

#   }}}

#   Tensors {{{

#     Boundaries {{{

#       Expectation {{{

class LeftExpectationBoundary(Tensor): # {{{
    dimension_names = ["operator","state_conjugate","state"]
    def absorb(self,state_site,operator_site):
        return type(self)(core.contract_sos_left(
            operator_site.right_dimension,
            self.data.transpose(),
            operator_site.index_table.transpose(),
            operator_site.matrix_table.transpose(),
            state_site.data.transpose()).transpose()
        )
# }}}

class RightExpectationBoundary(Tensor): # {{{
    dimension_names = ["operator","state","state_conjugate"]
    def absorb(self,state_site,operator_site):
        return type(self)(core.contract_sos_right(
            operator_site.left_dimension,self.data.transpose(),
            operator_site.index_table.transpose(),
            operator_site.matrix_table.transpose(),
            state_site.data.transpose()).transpose()
        )

# }}}

#       }}}

#       Overlap {{{

class LeftOverlapBoundary(Tensor): # {{{
    dimension_names = ["overlap","state"]
    def absorb(self,overlap_site,state_site):
        return type(self)(
            core.contract_vs_left(self.data.transpose(),
            overlap_site.data.transpose(),
            state_site.data.transpose()).transpose()
        )
# }}}

class RightOverlapBoundary(Tensor): # {{{
    dimension_names = ["state","overlap"]
    def absorb(self,overlap_site,state_site):
        return type(self)(
            core.contract_vs_right(self.data.transpose(),
            overlap_site.data.transpose(),
            state_site.data.transpose()).transpose()
        )
# }}}
# }}}

#     }}}

#     Sites {{{

class DenseOperatorSite(OperatorSiteTensor):
    pass

class OperatorSite(OperatorSiteTensor): # {{{
    def __init__(self,left_dimension,right_dimension,index_table,matrix_table): # {{{
        self.left_dimension = left_dimension
        self.right_dimension = right_dimension
        self.index_table = index_table
        self.matrix_table = matrix_table
        if self.index_table.ndim != 2:
            raise ValueError("index table must have rank 2")
        if self.matrix_table.ndim != 3:
            raise ValueError("matrix table must have rank 2")
        if self.index_table.shape[0] != self.matrix_table.shape[0]:
            raise ValueError("index table has {} entries but matrix table has {}".format(self.index_table.shape[0],self.matrix_table.shape[0]))
        self.number_of_matrices = self.matrix_table.shape[0]
        self.physical_dimension = self.physical_conjugate_dimension = self.matrix_table.shape[-1]
    # }}}

    @classmethod
    def build(cls,bandwidth_dimensions,components): # {{{
        bandwidth_dimension_names = [x[0] for x in bandwidth_dimensions]
        if bandwidth_dimension_names == ["left","right"]:
            flip = False
        elif bandwidth_dimension_names == ["right","left"]:
            flip = True
        else:
            raise ValueError("the only supported bandwidth dimension names are 'left' and 'right', but the given dimension names were {}".format(bandwidth_dimension_names))
        number_of_matrices = len(components)
        if number_of_matrices == 0:
            raise ValueError("there must be at least one component")
        index_table = ndarray((number_of_matrices,2),dtype=int)
        matrix_table = ndarray((number_of_matrices,) + components[0][1].shape,dtype=complex128)
        for (i,((x,y),component_value)) in enumerate(components):
            if flip:
                index_table[i,0] = y+1
                index_table[i,1] = x+1
            else:
                index_table[i,0] = x+1
                index_table[i,1] = y+1
            matrix_table[i] = component_value
        return OperatorSite(index_table=index_table,matrix_table=matrix_table,**dict((name + "_dimension",dimension) for (name,dimension) in bandwidth_dimensions))
    # }}}

    def formDenseTensor(self): # {{{
        operator = zeros([self.left_dimension,self.right_dimension,self.physical_dimension,self.physical_dimension,],dtype=complex128,order='F')
        for i in xrange(self.number_of_matrices):
            left_index, right_index = self.index_table[i]-1
            operator[left_index,right_index] += self.matrix_table[i]
        return DenseOperatorSite(operator)
    # }}}

    @staticmethod
    def random(left_dimension,right_dimension,physical_dimension,number_of_matrices=None,symmetric=False): # {{{
        if number_of_matrices is None:
            number_of_matrices = randint(2,left_dimension+right_dimension+1)
        sparse_operator_indices = array([
            randint(1,left_dimension+1,size=number_of_matrices),
            randint(1,right_dimension+1,size=number_of_matrices),
        ]).transpose()
        sparse_operator_matrices = crand(number_of_matrices,physical_dimension,physical_dimension)
        if symmetric:
            sparse_operator_matrices += sparse_operator_matrices.transpose(0,2,1).conj()
        return OperatorSite(
            index_table = sparse_operator_indices,
            matrix_table = sparse_operator_matrices,
            left_dimension = left_dimension,
            right_dimension = right_dimension
        )
    # }}}

    @classmethod
    def trivial(cls): # {{{
        return cls(1,1,ones((1,2),dtype=int),ones((1,1,1),dtype=complex128))
    # }}}
# }}}

class OverlapSite(SiteTensor):
    dimension_names = ["right","physical","left"]

class StateSite(StateSiteTensor): # {{{
    dimension_names = ["physical","left","right"]

    def formNormalizedOverlapSites(self,right_normalized_right_neighbor): # {{{
        return self.FormAndNormalizeOverlapSiteResult(*mapFunctions(
            (OverlapSite,OverlapSite,StateSite,OverlapSite),
            (x.transpose() for x in core.form_norm_overlap_tensors(self.data.transpose(),right_normalized_right_neighbor.data.transpose()))
        ))
    FormAndNormalizeOverlapSiteResult = \
        namedtuple("FormAndNormalizeOverlapSiteResult",
            ["left_normalized_left_overlap_site"
            ,"unnormalized_left_overlap_site"
            ,"unnormalized_right_state_site"
            ,"right_normalized_right_overlap_site"
            ]
        )
# }}}

    def formOverlapSite(self): # {{{
        return OverlapSite(core.form_overlap_site_tensor(self.data.transpose()).transpose())
    # }}}

    def normalizeAndDenormalize(self,direction,other): # {{{
        if direction == Direction.right:
            info, new_other, new_self = core.norm_denorm_going_left(other.data.transpose(),self.data.transpose())
        elif direction == Direction.left:
            info, new_self, new_other = core.norm_denorm_going_right(self.data.transpose(),other.data.transpose())
        else:
            raise ValueError("direction must be Direction.left or Direction.right, not {}".format(direction))
        if info != 0:
            raise Exception("Error code {} (!= 0) returned from the normalization routine.".format(info))
        return StateSite(new_self.transpose()), StateSite(new_other.transpose())
    # }}}

    @classmethod
    def random(cls,normalization=Normalization.none,**keywords): # {{{
        self = super(cls,cls).random(**keywords)
        if normalization == Normalization.middle:
            self.data /= norm(self.data)
        elif normalization == Normalization.left:
            self.data = normalize(self.data,StateSite.right_index)
        elif normalization == Normalization.right:
            self.data = normalize(self.data,StateSite.left_index)
        return self
    # }}}
# }}}

# }}}

#   }}}

# }}}

# Exports {{{
__all__ = [
    "OperatorSiteTensor",

    "LeftExpectationBoundary",
    "RightExpectationBoundary",
    "LeftOverlapBoundary",
    "RightOverlapBoundary",

    "DenseOperatorSite",
    "OperatorSite",
    "OverlapSite",
    "StateSite",
]
# }}}
