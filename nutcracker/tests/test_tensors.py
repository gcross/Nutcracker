#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1643: * @file test_tensors.py
#@+<< Imports >>
#@+node:gcross.20111109104457.1644: ** << Imports >>
from . import *
from ..tensors import SiteTensor

from copy import copy
from numpy import complex128, zeros
from random import randint,shuffle
#@-<< Imports >>

#@+others
#@+node:gcross.20111109104457.1645: ** Tests
#@+node:gcross.20111109104457.1646: *3* TestSiteTensor
class TestSiteTensor(TestCase):
    #@+others
    #@+node:gcross.20111109104457.1647: *4* test_build
    @with_checker
    def test_build(self,
        number_of_physical_dimensions=irange(0,2),
        number_of_bandwidth_dimensions=irange(0,4),
        number_of_components = irange(1,10)
    ):
        number_of_dimensions = number_of_bandwidth_dimensions + number_of_physical_dimensions
        if number_of_dimensions == 0:
            return
        bandwidth_dimension_names = ["A","B","C","D"][:number_of_bandwidth_dimensions]
        physical_dimension_names = ["physical","physical_conjugate"][:number_of_physical_dimensions]
        original_dimension_names = bandwidth_dimension_names + physical_dimension_names
        bandwidth_dimensions = [randint(1,5) for _ in xrange(number_of_bandwidth_dimensions)]
        physical_dimension = randint(1,5)
        physical_dimensions = [physical_dimension]*number_of_physical_dimensions
        dimensions = bandwidth_dimensions + physical_dimensions
        correct_data = zeros(dimensions,dtype=complex128)
        components = []
        for _ in xrange(number_of_components):
            bandwidth_indices = [randint(0,dimension-1) for dimension in bandwidth_dimensions]
            component_value = crand(*physical_dimensions)
            components.append((bandwidth_indices,component_value))
            correct_data[tuple(bandwidth_indices)] += component_value
        order = range(number_of_dimensions)
        shuffle(order)
        inverse_order = [order.index(i) for i in xrange(number_of_dimensions)]
        class MySiteTensor(SiteTensor):
            dimension_names = [original_dimension_names[i] for i in order]
        tensor = MySiteTensor.build(zip(bandwidth_dimension_names,bandwidth_dimensions),components)
        data = tensor.data.transpose(inverse_order)
        self.assertAllClose(data,correct_data)
    #@+node:gcross.20111109104457.1649: *4* test_simple
    @with_checker
    def test_simple(self,
        number_of_physical_dimensions=irange(0,2),
        number_of_bandwidth_dimensions=irange(0,4)
    ): 
        number_of_dimensions = number_of_bandwidth_dimensions + number_of_physical_dimensions
        if number_of_dimensions == 0:
            return
        bandwidth_dimension_names = ["A","B","C","D"][:number_of_bandwidth_dimensions]
        physical_dimension_names = ["physical","physical_conjugate"][:number_of_physical_dimensions]
        original_dimension_names = bandwidth_dimension_names + physical_dimension_names
        bandwidth_dimensions = [1]*number_of_bandwidth_dimensions
        physical_dimension = randint(1,5)
        physical_dimensions = [physical_dimension]*number_of_physical_dimensions
        dimensions = bandwidth_dimensions + physical_dimensions
        correct_data = zeros(dimensions,dtype=complex128)
        components = []
        for _ in xrange(1):
            bandwidth_indices = [randint(0,dimension-1) for dimension in bandwidth_dimensions]
            component_value = crand(*physical_dimensions)
            components.append((bandwidth_indices,component_value))
            correct_data[tuple(bandwidth_indices)] += component_value
        order = range(number_of_dimensions)
        shuffle(order)
        inverse_order = [order.index(i) for i in xrange(number_of_dimensions)]
        class MySiteTensor(SiteTensor):
            dimension_names = [original_dimension_names[i] for i in order]
        tensor = MySiteTensor.simple(components[0][1])
        data = tensor.data.transpose(inverse_order)
        self.assertAllClose(data,correct_data)
    #@-others
#@-others
#@-leo
