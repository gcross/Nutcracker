# Imports {{{
from numpy import tensordot
# }}}

# Functions {{{

def computeShapeFromGroupedOrder(old_shape,new_grouped_order): # {{{
    old_shape = self.shape
    new_shape = []
    current_index = 0
    while current_index < self.number_of_dimensions:
        number_of_indices_to_join = joins.get(current_index,1)
        new_shape.append(prod(old_shape[current_index:current_index+number_of_indices_to_join]))
        current_index += number_of_indices_to_join
    return self.data_.reshape(new_shape)
# }}}

def computeOrderAndShapeFromGroupedOrder(new_grouped_order): # {{{
    
# }}}

def contract(data_1,data_2,axes_1,axes_2): # {{{
    return tensordot(data_1,data_2,(axes_1,axes_2)
# }}}

def contractAndTranspose(data_1,data_2,axes_1,axes_2,new_order): # {{{
    return tensordot(data_1,data_2,(axes_1,axes_2).transpose(new_order)
# }}}

def contractAndTransposeAndJoin(data_1,data_2,axes_1,axes_2,new_grouped_order): # {{{
    new_order, new_shape = computeOrderAndShapeFromGroupedOrder(new_grouped_order)
    return tensordot(data_1,data_2,(axes_1,axes_2).transpose(new_order).reshape(new_shape))
# }}}

# }}}
