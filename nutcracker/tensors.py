# Imports {{{
from collections import defaultdict

from utils import Join, makeDataContractor
# }}}

# Metaclasses {{{

class MetaTensor(type): # {{{
    def __new__(cls,class_name,bases,data):
        if "index_names" in data:
            index_names = data["index_names"]
            data["rank"] = len(index_names)
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
            # Add properties to store the allowed joins and groups {{{
            data["_legal_joins"] = defaultdict(lambda: set())
            data["_legal_groupings"] = defaultdict(lambda: set())
            # }}}
        return type.__new__(cls,class_name,bases,data)
# }}}

# }}}

# Base Classes {{{

class Tensor(object): # {{{
    __metaclass__ = MetaTensor

  # Instance methods {{{

    def __init__(self,data): # {{{
        if(data.ndim != self.rank):
            raise ValueError("constructor was given a reference to a tensor of rank {}, when a tensor of rank {} was required".format(data.ndim,self.rank))
        self.data = data
        self.shape = data.shape
    # }}}

  # }}}

  # Properties {{{

    size = property(lambda self: product(self.shape))

  # }}}

  # Class methods {{{

    @classmethod
    def constructOrderedTuple(cls,*args,**keywords): # {{{
        if "conjugate" in keywords:
            conjugate = keywords["conjugate"]
            del keywords["conjugate"]
        else:
            conjugate = lambda x: x
        indices = cls.indices
        for name in keywords.keys():
            if name not in indices:
                raise ValueError("{} is not a recognized index for this tensor".format(name))
        if len(args) > cls.rank:
            raise ValueError("{} indices were provided for a tensor with only {}".format(len(args),cls.rank))
        ordered_tuple = list(args) + [None]*(cls.rank-len(args))
        for (name, value) in keywords.iteritems():
            index = indices[name]
            if ordered_tuple[index] is not None:
                raise ValueError("'{}' is specified both in the position and in the keyword arguments (respectively as {} and {})".format(name,ordered_tuple[index],value))
            else:
                ordered_tuple[index] = value
        for (name,conjugate_name) in cls.index_names_with_conjugate.iteritems():
            index = indices[name]
            conjugate_index = indices[conjugate_name]
            if ordered_tuple[conjugate_index] is None:
                ordered_tuple[conjugate_index] = conjugate(ordered_tuple[index])
            elif ordered_tuple[index] is not None and ordered_tuple[conjugate_index] != conjugate(ordered_tuple[index]):
                raise ValueError("the (conjugated) value for '{}' and its conjugate index '{}' must agree ({} != {})".format(name,conjugate_name,conjugate(ordered_tuple[index]),ordered_tuple[conjugate_index]))
        for index_name, value in zip(cls.index_names,ordered_tuple):
            if value is None:
                raise ValueError("missing a value for dimension {}".format(index_name))
        return tuple(ordered_tuple)
    # }}}

    @classmethod
    def getIndexForName(cls,name): # {{{
        try:
            return cls.indices[name]
        except KeyError:
            raise ValueError("tensor class {} does not have an index named '{}'".format(cls,name))
    # }}}

  # }}}

# }}}

# }}}

# Exceptions {{{

class IllegalGroupingError(ValueError): # {{{
    def __init__(self,tensor_class,index_name,grouping):
        ValueError.__init__(self,"it is not legal to group the following into index {} of {}: {}".format(index_name,tensor_class.__name__,"[" + ",".join('({},"{}")'.format(cls.__name__,index) for (cls,index) in grouping)))
        self.tensor_class = tensor_class
        self.index_name = index_name
        self.grouping = grouping
# }}}

class IllegalJoinError(ValueError): # {{{
    def __init__(self,left_tensor_class,left_index_name,right_tensor_class,right_index_name):
        ValueError.__init__(self,"it is not legal to join {}'s {} index to {}'s {} index".format(left_tensor_class.__name__,left_index_name,right_tensor_class.__name__,right_index_name))
        self.left_tensor_class = left_tensor_class
        self.left_index_name = left_index_name
        self.right_tensor_class = right_tensor_class
        self.right_index_name = right_index_name
# }}}

# }}}

# Functions {{{

def ensureGroupingIsLegal(tensor_class,index_name,grouping): # {{{
    if tuple(grouping) not in tensor_class._legal_groupings[index_name]:
        raise IllegalGroupingError(tensor_class,index_name,grouping)
# }}}

def ensureJoinIsLegal(left_tensor_class,left_index_name,right_tensor_class,right_index_name): # {{{
    if (left_index_name,right_index_name) not in left_tensor_class._legal_joins[right_tensor_class]:
        raise IllegalJoinError(left_tensor_class,left_index_name,right_tensor_class,right_index_name)
# }}}

def makeTensorContractor(tensor_classes,result_class,joins,final_groups): # {{{
  # Prelude: a small utility function for mapping tensor numbers to classes {{{
    def getClassFor(tensor_number):
        try:
            return tensor_classes[tensor_number]
        except KeyError:
            raise ValueError("tensor number {} is invalid;  only {} tensors arguments have been specified ({})".format(tensor_number,len(tensor_classes),tensor_classes))
  # }}}
  # First, check that all the joins are legal {{{
    translated_joins = []
    for join in joins:
        left_tensor_class = getClassFor(join.left_tensor_number)
        right_tensor_class = getClassFor(join.right_tensor_number)
        left_tensor_indices = []
        right_tensor_indices = []
        for (left_tensor_index_name,right_tensor_index_name) in zip(join.left_tensor_indices,join.right_tensor_indices):
            left_tensor_indices.append(left_tensor_class.getIndexForName(left_tensor_index_name))
            right_tensor_indices.append(right_tensor_class.getIndexForName(right_tensor_index_name))
            ensureJoinIsLegal(left_tensor_class,left_tensor_index_name,right_tensor_class,right_tensor_index_name)
        translated_joins.append(Join(
            join.left_tensor_number,
            left_tensor_indices,
            join.right_tensor_number,
            right_tensor_indices
        ))
    joins = translated_joins
    del translated_joins
  # }}}
  # Next, check that all the groups are legal {{{
    translated_final_groups = [None] * result_class.rank
    for result_index_name, group in final_groups.iteritems():
        grouping = []
        translated_group = []
        for (tensor_number,tensor_index_name) in group:
            tensor_class = getClassFor(tensor_number)
            grouping.append((tensor_class,tensor_index_name))
            tensor_index = tensor_class.getIndexForName(tensor_index_name)
            translated_group.append((tensor_number,tensor_index))
        ensureGroupingIsLegal(result_class,result_index_name,grouping)
        translated_final_groups[result_class.indices[result_index_name]] = translated_group
    for i, translated_group in enumerate(translated_final_groups):
        if translated_group is None:
            raise ValueError("Missing a final group for index {} of the result class {}".format(result_class.index_names[i],result_class.__name__))
    final_groups = translated_final_groups
    del translated_final_groups
  # }}}
  # Construct and return the contractor {{{
    function_source = "\n    ".join([
        "def contract(" + ",".join("_{}".format(i) for i in xrange(len(tensor_classes))) + "):",
            '"""',
    ] + [   "_{} - a {}".format(i,cls.__name__) for (i,cls) in enumerate(tensor_classes)] + [
            '"""',
    ] + [   'if not isinstance(_{i},_{i}_class): raise TypeError("argument {i} must be a tensor of class {cls}")'.format(i=i,cls=cls.__name__) for (i,cls) in enumerate(tensor_classes)] + [
            "return wrapInTensor(contractData(" + ",".join("_{}.data".format(i) for i in range(len(tensor_classes))) + "))"
    ])
    contractData = makeDataContractor(joins,final_groups,tensor_ranks=[cls.rank for cls in tensor_classes])
    captured_definitions = {}
    environment = {"ValueError": ValueError,"contractData": contractData,"wrapInTensor":result_class}
    for i, tensor_class in enumerate(tensor_classes):
        environment["_{}_class".format(i)] = tensor_class
    exec function_source in environment, captured_definitions
    contract = captured_definitions["contract"]
    contract.outer_source = function_source
    contract.inner_source = contractData.source
    return contract
  # }}}
# }}}

def registerLegalGrouping(tensor_class,index_name,*group): # {{{
    if index_name not in tensor_class.index_names:
        raise ValueError('"{}" is not an index of {}'.format(index_name,tensor_class.__name__))
    tensor_class._legal_groupings[index_name].add(group)
# }}}

def registerLegalJoin(left_tensor_class,left_index_name,right_tensor_class,right_index_name): # {{{
    left_tensor_class._legal_joins[right_tensor_class].add((left_index_name,right_index_name))
    right_tensor_class._legal_joins[left_tensor_class].add((right_index_name,left_index_name))
# }}}

# }}}

# Exports {{{
__all__ = [
  # Classes {{{
    # Metaclasses {{{
    "MetaTensor",
    # }}}
    # Base classes {{{
    "Tensor",
    # }}}
  # }}}
  # Exceptions {{{
    "IllegalGroupingError",
    "IllegalJoinError",
  # }}}
  # Functions {{{
    "ensureGroupingIsLegal",
    "ensureJoinIsLegal",
    "makeTensorContractor",
    "registerLegalGrouping",
    "registerLegalJoin",
  # }}}
]
# }}}
