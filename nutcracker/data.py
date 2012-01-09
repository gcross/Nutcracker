# Imports {{{

from numpy import ndarray, prod

# }}}

# Classes {{{

class DenseData(object): # {{{

  # Initialization {{{

    def __init__(self,**keywords): # {{{
        self._data = ndarray(**keywords)
    # }}}

    @classmethod
    def filled(cls,fill,**keywords): # {{{
        self = cls(**keywords)
        self[...] = fill
        return self
    # }}}
    
    @classmethod
    def random(cls,**keywords): # {{{
        self = cls(**keywords)
        self[...] = crand(*self.shape)
        return self
    # }}}

  # }}}

  # Informational {{{

    number_of_dimensions = property(lambda self: self.data_.ndim)

    shape = property(lambda self: self.data_.shape)

  # }}}

  # Index manipulation {{{

    def join(self,first_index_to_join,number_of_indices_to_join): # {{{
        return self.joinMany({first_index_to_join:number_of_indices_to_join})
    # }}}

    def joinMany(self,joins): # {{{
        old_shape = self.shape
        new_shape = []
        current_index = 0
        while current_index < self.number_of_dimensions:
            number_of_indices_to_join = joins.get(current_index,1)
            new_shape.append(prod(old_shape[current_index:current_index+number_of_indices_to_join]))
            current_index += number_of_indices_to_join
        return self.data_.reshape(new_shape)
    # }}}

    def transpose(self,new_order): # {{{
        self._data = self._data.transpose(new_order)
    # }}}

    def transposeAndJoin(self,new_grouped_order): #{{{
        new_order = sum(new_grouped_order)
        new_joins = {}
        current_index = 0
        for group in new_grouped_order:
            number_of_indices = len(group)
            new_joins[current_index] = number_of_indices
            current_index += number_of_indices
        return self.transpose(new_order).joinMany(new_joins)
    # }}}

  # }}}

# }}}

# }}}

# Functions {{{

def contract(data_1,data_2,axes_1,axes_2): # {{{
    return DenseData(tensordot(data_1._data,data_2._data,(axes_1,axes_2)))
# }}}

# }}}
