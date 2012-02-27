# Imports {{{
# }}}

# Metaclasses {{{

class MetaTensor(type): # {{{
    def __new__(cls,class_name,bases,data):
        if "index_names" in data:
            index_names = data["index_names"]
            # Check that no dimension name is repeated {{{
            observed_index_names = set()
            for name in index_names:
                if name in observed_index_names:
                    raise ValueError("repeated index name '{}'".format(name))
                else:
                    observed_index_names.add(name)
            # }}}
            # Check that conjugated dimensions have a non-conjugated partner {{{
            index_names_with_conjugate = {}
            for name in index_names:
                if name.endswith("_conjugate"):
                    non_conjugate_name = name[:-10]
                    if non_conjugate_name not in observed_index_names:
                        raise ValueError("index name {} does not have a non-conjugated partner")
                    else:
                        index_names_with_conjugate[non_conjugate_name] = name
            data["index_names_with_conjugate"] = index_names_with_conjugate
            # }}}
            # Add properties for each of the dimensions {{{
            indices = {}
            for (index,name) in enumerate(data["index_names"]):
                indices[name] = index
                data[name + "_index"] = index
                data[name + "_dimension"] = property(lambda self: self.shape[index])
            data["indices"] = indices
            # }}}
        return type.__new__(cls,class_name,bases,data)
# }}}

class MetaSiteTensor(MetaTensor): # {{{
    def __new__(cls,class_name,bases,data):
        if "dimension_names" in data:
            physical_dimension_names = []
            physical_dimension_indices = {}
            physical_indices = []
            bandwidth_dimension_names = []
            bandwidth_dimension_indices = {}
            bandwidth_indices = []
            dimension_names = data["dimension_names"]
            for (index,name) in enumerate(dimension_names):
                if name.startswith("physical"):
                    physical_dimension_names.append(name)
                    physical_dimension_indices[name] = index
                    physical_indices.append(index)
                else:
                    bandwidth_dimension_names.append(name)
                    bandwidth_dimension_indices[name] = index
                    bandwidth_indices.append(index)
            for varname in [
                "physical_dimension_indices",
                "physical_dimension_names",
                "physical_indices",
                "bandwidth_dimension_indices",
                "bandwidth_dimension_names",
                "bandwidth_indices",
            ]:
                data[varname] = locals()[varname]
            data["number_of_physical_dimensions"] = len(physical_dimensions)
            data["number_of_bandwidth_dimensions"] = len(bandwidth_dimensions)
            data["physical_indices_are_transposed"] = (
                "physical" in dimensions and
                "physical_conjugate" in dimensions and
                physical_dimension_indices["physical"] < physical_dimension_indices["physical_conjugate"]
            )
        return MetaTensor.__new__(cls,class_name,bases,data)
# }}}

# }}}

# Base Classes {{{

class Tensor(object):# {{{
    __metaclass__ = MetaTensor

  # Instance methods {{{

    def __init__(self,data): # {{{
        if(self.data.ndim != self.number_of_dimensions):
            raise ValueError("constructor was given a reference to a tensor of rank {}, when a tensor of rank {} was required".format(self.data.ndim,self.number_of_dimensions))
        self.data = data
        self.shape = data.shape
    # }}}

  # }}}

  # Properties {{{

    size = property(lambda self: product(self.shape))

  # }}}

  # Class methods {{{

    @classmethod
    def constrctShape(cls,*args,**keywords): # {{{
        for (name,conjugated_name) in cls.dimension_names_with_conjugate:
            if name in keywords:
                keywords[conjugated_name] = keywords[name]
        dimension_indices = self.dimension_indices
        for name in keywords.keys():
            if name not in dimension_indices:
                raise ValueError("{} is not a recognized dimension for this tensor".format(name))
        if len(args) > self.number_of_dimensions:
            raise ValueError("{} dimensions were provided for a tensor with only {}".format(len(args),self.number_of_dimensions))
        shape = list(args) + [None]*(self.number_of_dimensions-len(args))
        for (name, dimension) in keywords:
            index = dimension_indices[name]
            if shape[index] is not None:
                raise ValueError("dimension '{}' is specified both in the position and in the keyword arguments (respectively as {} and {})".format(name,shape[index],dimension))
            else:
                shape[index] = dimension
        for index, dimension in shape:
            if dimension is None:
                raise ValueError("missing a value for dimension {}".format(self.dimension_names[index]))
        return tuple(shape)
    # }}}

  # }}}

# }}}

class SiteTensor(Tensor): # {{{
    __metaclass__ = MetaSiteTensor

  # Instance methods {{{

    def __init__(self,data): # {{{
        Tensor.__init__(self,data)
        shape = self.shape
        self.bandwidth_dimensions = tuple(shape[index] for index in self.bandwidth_dimension_indices)
        self.physical_dimensions = tuple(shape[index] for index in self.physical_dimension_indices)
    # }}}

  # }}}

# }}}

# }}}

# Exports {{{
__all__ = [
  # Classes {{{
    # Metaclasses {{{
    "MetaTensor",
    "MetaSiteTensor",
    # }}}
    # Base classes {{{
    "Tensor",
    "SiteTensor",
    # }}}
  # }}}
]
# }}}
