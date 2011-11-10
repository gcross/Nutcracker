#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1830: * @file operators.py
#@+<< License >>
#@+node:gcross.20111109104457.1831: ** << License >>
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20111109104457.1832: ** << Imports >>
from numpy import array, complex128

from ..tensors import OperatorSite
#@-<< Imports >>

#@+others
#@+node:gcross.20111109104457.1834: ** Classes
#@+node:gcross.20111109104457.1835: *3* Operator
class Operator(object):
    #@+others
    #@+node:gcross.20111109104457.1837: *4* __init__
    def __init__(self,operator_site,left_operator_boundary_vector,right_operator_boundary_vector):
        if operator_site.left_dimension != operator_site.right_dimension:
            raise ValueError("the left and right dimensions of the operator site tensor must match in an infinite chain ({} != {})".format(operator_site.left_dimension,operator.right_dimension))
        if len(left_operator_boundary_vector) != operator_site.left_dimension:
            raise ValueError("the length of the left boundary vector does not match the left dimension of the operator site tensor ({} != {})".format(len(left_operator_boundary_vector),operator.left_dimension))
        if len(right_operator_boundary_vector) != operator_site.right_dimension:
            raise ValueError("the length of the right boundary vector does not match the right dimension of the operator site tensor ({} != {})".format(len(right_operator_boundary),operator_site.right_dimension))
        self.operator_site = operator_site
        self.left_operator_boundary_vector = array(left_operator_boundary_vector,dtype=complex128)
        self.right_operator_boundary_vector = array(left_operator_boundary_vector,dtype=complex128)
    #@+node:gcross.20111109104457.1836: *4* (left/right/physical)_dimension
    left_dimension = property(lambda self: self.operator_site.left_dimension)
    right_dimension = property(lambda self: self.operator_site.right_dimension)
    physical_dimension = property(lambda self: self.operator_site.physical_dimension)
    #@+node:gcross.20111109104457.1838: *4* trivial
    @classmethod
    def trivial(cls):
        return cls(OperatorSite.trivial(),[1],[1])
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111109104457.1833: ** << Exports >>
__all__ = [
    "Operator",
]
#@-<< Exports >>
#@-leo
