# Imports {{{
from collections import defaultdict, namedtuple
from itertools import izip
from numpy import prod
import operator
# }}}

# Classes {{{

class VirtualShape(object): # {{{
    def __init__(self,entries):
        self.entries = entries
        self.size = reduce(operator.mul,(entry.size for entry in entries),1)
    def sizes(self):
        return [entry.size for entry in self.indices]
# }}}

VirtualShapeEntry = namedtuple("VirtualShapeEntry",["index","size","sparsity"])

# }}}

# Functions {{{

def reconcileVirtualShapes(virtual_shape_1,virtual_shape_2): # {{{
    if virtual_shape_1.size != virtual_shape_2.size:
        raise ValueError("unable to reconcile group of size {} with group of size {}".format(virtual_shape_1.size,virtual_shape_1.size))
    groups = [map(reconcileVirtualShapes.Element.fromVirtualShapeEntry,reversed(virtual_shape.entries)) for virtual_shape in [virtual_shape_1,virtual_shape_2]]
    reconciled_virtual_shapes = [[],[]]
    virtual_shape_splits = [defaultdict(lambda: []) for _ in xrange(2)]
    while len(groups[0]) > 0:
        elements = [group.pop() for group in groups]
        for a, b in [(0,1),(1,0)]:
            if elements[a].size > elements[b].size:
                if elements[a].size % elements[b].size != 0:
                    raise ValueError("virtual shapes {} and {} cannot be reconciled into a common product of factors ({} % {} != 0)".format(virtual_shape_1.sizes(),virtual_shape_2.sizes(),elements[a].size,elements[b].size))
                groups[a].append(
                    reconcileVirtualShapes.Element(
                        index=elements[a].index,
                        subindex=elements[a].subindex+1,
                        size=elements[a].size/elements[b].size,
                        sparsity=elements[a].sparsity
                    )
                )
                elements[a].size = elements[b].size
        assert elements[0].size == elements[1].size
        for (element,reconciled_virtual_shape,virtual_shape_split) in izip(elements,reconciled_virtual_shapes,virtual_shape_splits):
            reconciled_virtual_shape.append(VirtualShapeEntry(
                index=(element.index,element.subindex),
                size=element.size,
                sparsity=element.sparsity
            ))
            virtual_shape_split[element.index].append(element.size)
    for group in groups:
        assert len(group) == 0
    reconciled_virtual_shapes = map(VirtualShape,reconciled_virtual_shapes)
    assert reconciled_virtual_shapes[0].size == reconciled_virtual_shapes[1].size
    return reconciled_virtual_shapes, virtual_shape_splits

class reconcileVirtualShapes_Element(object):
    def __init__(self,index,subindex,size,sparsity):
        self.index = index
        self.subindex = subindex
        self.size = size
        self.sparsity = sparsity
    @classmethod
    def fromVirtualShapeEntry(cls,virtual_shape_entry):
        return cls(
            index = virtual_shape_entry.index,
            subindex = 0,
            size = virtual_shape_entry.size,
            sparsity = virtual_shape_entry.sparsity,
        )
reconcileVirtualShapes.Element = reconcileVirtualShapes_Element
del reconcileVirtualShapes_Element

# }}}

# }}}

# Exports {{{
__all__ = [
    "VirtualShape",
    "VirtualShapeEntry",

    "reconcileVirtualShapes",
]
# }}}
