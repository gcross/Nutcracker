#@+leo-ver=5-thin
#@+node:gcross.20111217234718.1783: * @file sparse.py
#@+<< License >>
#@+node:gcross.20111217234718.1784: ** << License >>
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20111217234718.1785: ** << Imports >>
from collections import defaultdict, namedtuple
from itertools import izip
#@-<< Imports >>

#@+others
#@+node:gcross.20111217234718.1789: ** Tuples
#@+node:gcross.20111217234718.1792: *3* VirtualShape
class VirtualShape(object):
    def __init__(self,entries):
        self.entries = entries
        self.size = prod(entry.size for entry in entries)
    def sizes(self):
        return [entry.size for entry in self.indices]
#@+node:gcross.20111217234718.1790: *3* VirtualShapeEntry
VirtualShapeEntry = namedtuple("VirtualShapeEntry",["index","size"])
#@+node:gcross.20111217234718.1788: ** Functions
#@+node:gcross.20111217234718.1787: *3* reconcileVirtualShapes
def reconcileVirtualShapes(virtual_shape_1,virtual_shape_2):
    class Element(object)
        def __init__(self,virtual_shape_entry):
            self.index = virtual_shape_entry.index
            self.subindex = 0
            self.size = virtual_shape_entry.size
    if virtual_shape_1.size != virtual_shape_2.size:
        raise ValueError("unable to reconcile group of size {} with group of size {}".format(virtual_shape_1.size,virtual_shape_1.size))
    groups = [map(Element,reversed(virtual_shape.entries)) for virtual_shape in [virtual_shape_1,virtual_shape_2]]
    reconciled_virtual_shapes = [[],[]]
    virtual_shape_splits = [defaultdict(lambda: []) for _ in xrange(2)]
    while len(groups[0]) > 0:
        elements = [group.pop() for group in groups]
        for a, b in [(0,1),(1,0)]:
            if elements[a].size > elements[b].size:
                if elements[a].size % elements[b].size != 0:
                    raise ValueError("virtual shapes {} and {} cannot be reconciled into a common product of factors ({} % {} != 0)".format(virtual_shape_1.sizes(),virtual_shape_2.sizes(),elements[a].size,elements[b].size)
                groups[a].push(Element(index=elements[a].index,subindex=elements[a].subindex+1,size=elements[a].size/elements[b].size))
                elements[a].size = elements[1].size
        assert element[0].size == element[1].size
        for (element,reconciled_virtual_shape,virtual_shape_split) in izip(elements,reconciled_virtual_shapes,virtual_shape_splits):
            reconciled_virtual_shape.append(VirtualShapeEntry(index=(element.index,element.subindex),size=element.size))
            virtual_shape_split[element.index].append(element.size)
    for group in groups:
        assert len(group) == 0
    reconciled_virtual_shapes = map(VirtualShape,reconciled_virtual_shapes)
    assert reconciled_virtual_shapes[0].size == reconciled_virtual_shapes[1].size
#@-others

#@+<< Exports >>
#@+node:gcross.20111217234718.1786: ** << Exports >>
__all__ = [
]
#@-<< Exports >>
#@-leo
