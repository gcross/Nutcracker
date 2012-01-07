# Imports {{{
from copy import copy
from numpy import prod
import operator
from paycheck import generator
from paycheck import *
from random import randint
from uuid import uuid4

from . import *
from ..enumerations import *
from ..sparse import *
# }}}

# Helper functions {{{

def primesUpTo(inclusive_upper_bound): # {{{
    primes = []
    current = 2
    while current <= inclusive_upper_bound:
        is_prime = True
        for prime in primes:
            if current % prime == 0:
                is_prime = False
                break
        if is_prime:
            primes.append(current)
        current += 1
    return primes
# }}}

def randomVirtualShapeFromPrimes(primes): # {{{
    primes = copy(primes)
    entries = []
    while len(primes) > 0:
        n = randint(0,len(primes))
        entries.append(VirtualShapeEntry(
            index=uuid4(),
            size=reduce(operator.mul,primes[:n],1),
            sparsity=Sparsity.randomChoice()
        ))
        primes = primes[n:]
    return VirtualShape(entries)
# }}}

# }}}

# Generators {{{

class PairOfVirtualShapesGenerator(generator.PayCheckGenerator): # {{{
    def __init__(self,upper_bound_on_primes=20):
        self.prime_list_generator = self.get([choiceof(primesUpTo(upper_bound_on_primes))])
    def __next__(self):
        primes = next(self.prime_list_generator)
        return (randomVirtualShapeFromPrimes(primes),randomVirtualShapeFromPrimes(primes))
pair_of_virtual_shapes = PairOfVirtualShapesGenerator
# }}}

# }}}

# Tests {{{

class TestPairOfVirtualShapesGenerator(TestCase): # {{{
    @with_checker
    def test_both_shapes_have_the_same_size(self,pair_of_virtual_shapes=pair_of_virtual_shapes): # {{{
        self.assertEqual(pair_of_virtual_shapes[0].size,pair_of_virtual_shapes[1].size)
    # }}}
# }}}

class TestReconcileVirtualShapes(TestCase): # {{{
    @with_checker
    def test_correct_split_total(self,pair_of_virtual_shapes=pair_of_virtual_shapes): # {{{
        (_,splits) = reconcileVirtualShapes(*pair_of_virtual_shapes)
        for (virtual_shape, split) in zip(pair_of_virtual_shapes,splits):
            for entry in virtual_shape.entries:
                self.assertEqual(entry.size,reduce(operator.mul,split[entry.index],1))
    # }}}

    @with_checker
    def test_size_unchanged_by_reconciliation(self,pair_of_virtual_shapes=pair_of_virtual_shapes): # {{{
        (pair_of_reconciled_virtual_shapes,_) = reconcileVirtualShapes(*pair_of_virtual_shapes)
        for (virtual_shape,reconciled_virtual_shape) in zip(pair_of_virtual_shapes,pair_of_reconciled_virtual_shapes):
            self.assertEqual(virtual_shape.size,reconciled_virtual_shape.size)
    # }}}

    @with_checker
    def test_entries_matich_after_reconciliation(self,pair_of_virtual_shapes=pair_of_virtual_shapes): # {{{
        (pair_of_reconciled_virtual_shapes,_) = reconcileVirtualShapes(*pair_of_virtual_shapes)
        for (entry_1,entry_2) in zip(*(virtual_shape.entries for virtual_shape in pair_of_reconciled_virtual_shapes)):
            self.assertEqual(entry_1.size,entry_2.size)
    # }}}

# }}}

# }}}
