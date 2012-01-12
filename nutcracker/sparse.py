# Imports {{{
from collections import defaultdict, namedtuple
from itertools import izip
from numpy import prod
import operator
# }}}

# Classes {{{

class VirtualIndex(tuple): # {{{
    def __init__(self,*args):
        super(VirtualIndex,self).__init__(*args)
        self.size = reduce(operator.mul,(entry.size for entry in self),1)
# }}}

VirtualIndexEntry = namedtuple("VirtualIndexEntry",["index","size","sparsity"])

class VirtualShape(tuple): # {{{
    def __init__(self,*args):
        super(VirtualShape,self).__init__(*args)
        self.shape = tuple(index.size for index in self)
        self.size = reduce(operator.mul,self.shape,1)
# }}}

# }}}

# Functions {{{

def reconcileVirtualIndices(virtual_shape_1,virtual_shape_2): # {{{
    if virtual_shape_1.size != virtual_shape_2.size:
        raise ValueError("unable to reconcile group of size {} with group of size {}".format(virtual_shape_1.size,virtual_shape_1.size))
    groups = [map(reconcileVirtualIndices.Element.fromVirtualIndexEntry,reversed(virtual_shape)) for virtual_shape in [virtual_shape_1,virtual_shape_2]]
    reconciled_virtual_shapes = [[],[]]
    virtual_shape_splits = [defaultdict(lambda: []) for _ in xrange(2)]
    while len(groups[0]) > 0:
        elements = [group.pop() for group in groups]
        for a, b in [(0,1),(1,0)]:
            if elements[a].size > elements[b].size:
                if elements[a].size % elements[b].size != 0:
                    raise ValueError("virtual shapes {} and {} cannot be reconciled into a common product of factors ({} % {} != 0)".format(virtual_shape_1.sizes(),virtual_shape_2.sizes(),elements[a].size,elements[b].size))
                groups[a].append(
                    reconcileVirtualIndices.Element(
                        index=elements[a].index,
                        subindex=elements[a].subindex+1,
                        size=elements[a].size/elements[b].size,
                        sparsity=elements[a].sparsity
                    )
                )
                elements[a].size = elements[b].size
        assert elements[0].size == elements[1].size
        for (element,reconciled_virtual_shape,virtual_shape_split) in izip(elements,reconciled_virtual_shapes,virtual_shape_splits):
            reconciled_virtual_shape.append(VirtualIndexEntry(
                index=(element.index,element.subindex),
                size=element.size,
                sparsity=element.sparsity
            ))
            virtual_shape_split[element.index].append(element.size)
    for group in groups:
        assert len(group) == 0
    reconciled_virtual_shapes = map(VirtualIndex,reconciled_virtual_shapes)
    assert reconciled_virtual_shapes[0].size == reconciled_virtual_shapes[1].size
    return reconciled_virtual_shapes, virtual_shape_splits

class reconcileVirtualIndices_Element(object):
    def __init__(self,index,subindex,size,sparsity):
        self.index = index
        self.subindex = subindex
        self.size = size
        self.sparsity = sparsity
    @classmethod
    def fromVirtualIndexEntry(cls,virtual_shape_entry):
        return cls(
            index = virtual_shape_entry.index,
            subindex = 0,
            size = virtual_shape_entry.size,
            sparsity = virtual_shape_entry.sparsity,
        )
reconcileVirtualIndices.Element = reconcileVirtualIndices_Element
del reconcileVirtualIndices_Element

# }}}

# }}}

# Exports {{{
__all__ = [
    "VirtualIndex",
    "VirtualIndexEntry",

    "reconcileVirtualIndices",
]
# }}}
