# Imports {{{
from numpy import identity, tensordot, zeros
from paycheck import *
from random import randint

from .._1d.enumerations import *
from .._1d.tensors import *
from ..utils import *

from . import *
# }}}

class TestLeftExpectationBoundary(TestCase): # {{{
    # test absorb {{{
    @prependContractor(
        ['L','O','S','S*'],
        [(('L',LeftExpectationBoundary.operator_index),('O',OperatorSite.left_index))
        ,(('L',LeftExpectationBoundary.state_index),('S',StateSite.left_index))
        ,(('L',LeftExpectationBoundary.state_conjugate_index),('S*',StateSite.left_index))
        ,(('O',OperatorSite.physical_index),('S',StateSite.physical_index))
        ,(('O',OperatorSite.physical_conjugate_index),('S*',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.right_index)]
         ,'state_conjugate':[('S*',StateSite.right_index)]
         ,'operator':[('O',OperatorSite.right_index)]
         }[name] for name in LeftExpectationBoundary.dimension_names
        ]
    )
    @with_checker
    def test_absorb(contract,self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_operator_dimension=irange(1,4),
        right_operator_dimension=irange(1,4),
    ):
        L = LeftExpectationBoundary.random(
            state_dimension = left_state_dimension,
            operator_dimension = left_operator_dimension,
        )
        O = OperatorSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_operator_dimension,
            right_dimension = right_operator_dimension,
        )
        S = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
        )
        self.assertAllClose(
            L.absorb(state_site=S,operator_site=O).data,
            contract(L.data,O.formDenseTensor().data,S.data,S.data.conj())
        )
    # }}}
# }}}

class TestLeftOverlapBoundary(TestCase): # {{{
    # test absorb {{{
    @prependContractor(
        ['L','V','S'],
        [(('L',LeftOverlapBoundary.overlap_index),('V',OverlapSite.left_index))
        ,(('L',LeftOverlapBoundary.state_index),('S',StateSite.left_index))
        ,(('V',OverlapSite.physical_index),('S',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.right_index)]
         ,'overlap':[('V',OverlapSite.right_index)]
         }[name] for name in LeftOverlapBoundary.dimension_names
        ]
    )
    @with_checker
    def test_absorb(contract,self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_overlap_dimension=irange(1,4),
        right_overlap_dimension=irange(1,4),
    ):
        L = LeftOverlapBoundary.random(
            state_dimension = left_state_dimension,
            overlap_dimension = left_overlap_dimension,
        )
        V = OverlapSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_overlap_dimension,
            right_dimension = right_overlap_dimension,
        )
        S = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
        )
        self.assertAllClose(
            L.absorb(overlap_site=V,state_site=S).data,
            contract(L.data,V.data,S.data)
        )
    # }}}
# }}}

class TestOperatorSite(TestCase): # {{{
    @with_checker
    def test_build(self, # {{{
        left_dimension=irange(1,5),
        right_dimension=irange(1,5),
        physical_dimension=irange(1,5),
        number_of_components=irange(1,10),
        flip=bool
    ):
        bandwidth_dimensions = [("left",left_dimension),("right",right_dimension)]
        if flip:
            bandwidth_dimensions.reverse()
        components = []
        for _ in xrange(number_of_components):
            bandwidth_indices = [randint(0,dimension-1) for (_,dimension) in bandwidth_dimensions]
            component_value = crand(physical_dimension,physical_dimension)
            components.append((bandwidth_indices,component_value))
        self.assertAllClose(
            OperatorSite.build(bandwidth_dimensions,components).formDenseTensor().data,
            DenseOperatorSite.build(bandwidth_dimensions,components).data,
        )
    # }}}

    @with_checker
    def test_simple(self, # {{{
        physical_dimension=irange(1,5),
        flip=bool
    ):
        left_dimension = 1
        right_dimension = 1
        number_of_components = 1
        bandwidth_dimensions = [("left",left_dimension),("right",right_dimension)]
        if flip:
            bandwidth_dimensions.reverse()
        components = []
        for _ in xrange(number_of_components):
            bandwidth_indices = [randint(0,dimension-1) for (_,dimension) in bandwidth_dimensions]
            component_value = crand(physical_dimension,physical_dimension)
            components.append((bandwidth_indices,component_value))
        self.assertAllClose(
            OperatorSite.simple(components[0][1]).formDenseTensor().data,
            DenseOperatorSite.simple(components[0][1]).data,
        )
    # }}}
# }}}

class TestRightExpectationBoundary(TestCase): # {{{
    # test absorb {{{
    @prependContractor(
        ['R','O','S','S*'],
        [(('R',RightExpectationBoundary.operator_index),('O',OperatorSite.right_index))
        ,(('R',RightExpectationBoundary.state_index),('S',StateSite.right_index))
        ,(('R',RightExpectationBoundary.state_conjugate_index),('S*',StateSite.right_index))
        ,(('O',OperatorSite.physical_index),('S',StateSite.physical_index))
        ,(('O',OperatorSite.physical_conjugate_index),('S*',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.left_index)]
         ,'state_conjugate':[('S*',StateSite.left_index)]
         ,'operator':[('O',OperatorSite.left_index)]
         }[name] for name in RightExpectationBoundary.dimension_names
        ]
    )
    @with_checker
    def test_absorb(contract,self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_operator_dimension=irange(1,4),
        right_operator_dimension=irange(1,4),
    ):
        R = RightExpectationBoundary.random(
            state_dimension = right_state_dimension,
            operator_dimension = right_operator_dimension,
        )
        O = OperatorSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_operator_dimension,
            right_dimension = right_operator_dimension,
        )
        S = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
        )
        self.assertAllClose(
            R.absorb(state_site=S,operator_site=O).data,
            contract(R.data,O.formDenseTensor().data,S.data,S.data.conj())
        )
    # }}}
# }}}

class TestLeftOverlapBoundary(TestCase): # {{{
    # test absorb {{{
    @prependContractor(
        ['R','V','S'],
        [(('R',RightOverlapBoundary.overlap_index),('V',OverlapSite.right_index))
        ,(('R',RightOverlapBoundary.state_index),('S',StateSite.right_index))
        ,(('V',OverlapSite.physical_index),('S',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.left_index)]
         ,'overlap':[('V',OverlapSite.left_index)]
         }[name] for name in RightOverlapBoundary.dimension_names
        ]
    )
    @with_checker
    def test_absorb(contract,self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_overlap_dimension=irange(1,4),
        right_overlap_dimension=irange(1,4),
    ):
        R = RightOverlapBoundary.random(
            state_dimension = right_state_dimension,
            overlap_dimension = right_overlap_dimension,
        )
        V = OverlapSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_overlap_dimension,
            right_dimension = right_overlap_dimension,
        )
        S = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
        )
        self.assertAllClose(
            R.absorb(overlap_site=V,state_site=S).data,
            contract(R.data,V.data,S.data)
        )
    # }}}
# }}}

class TestStateSite(TestCase): # {{{
    @with_checker
    def test_formNormalizedOverlapSites(self, # {{{
        left_physical_dimension = irange(2,4),
        right_physical_dimension = irange(2,4),
        leftmost_bandwidth_dimension = irange(2,4),
        middle_bandwidth_dimension = irange(2,4),
        rightmost_bandwidth_dimension = irange(2,4),
    ):
        unnormalized_left_state_site = StateSite.random(
            physical_dimension = left_physical_dimension,
            left_dimension = leftmost_bandwidth_dimension,
            right_dimension = middle_bandwidth_dimension,
        )
        right_normalized_right_state_site = StateSite.random(
            physical_dimension = right_physical_dimension,
            left_dimension = middle_bandwidth_dimension,
            right_dimension = rightmost_bandwidth_dimension,
        )
        result = unnormalized_left_state_site.formNormalizedOverlapSites(right_normalized_right_state_site)
        self.assertAllClose(
            result.unnormalized_left_overlap_site.data,
            unnormalized_left_state_site.data.transpose(2,0,1).conj()
        )
        self.assertAllClose(
            result.right_normalized_right_overlap_site.data,
            right_normalized_right_state_site.data.transpose(2,0,1).conj()
        )
        self.assertAllClose(
            tensordot(
                unnormalized_left_state_site.data,
                right_normalized_right_state_site.data,
                (StateSite.right_index,StateSite.left_index)
            ),
            tensordot(
                result.left_normalized_left_overlap_site.data.conj().transpose(1,2,0),
                result.unnormalized_right_state_site.data,
                (StateSite.right_index,StateSite.left_index)
            )
        )
        self.assertNormalized(result.left_normalized_left_overlap_site.data,OverlapSite.right_index)
    # }}}

    @with_checker
    def test_formOverlapSite(self, # {{{
        physical_dimension = irange(1,5),
        left_dimension = irange(1,5),
        right_dimension = irange(1,5)
    ):
        state_site = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
        )
        self.assertAllClose(
            state_site.formOverlapSite().data,
            state_site.data.conj().transpose(2,0,1)
        )
    # }}}

    @with_checker
    def test_normalizeAndDenormalize(self, # {{{
        direction=Direction,
        leftmost_dimension=irange(3,9),
        middle_dimension=irange(3,9),
        rightmost_dimension=irange(3,9),
    ):
        left_site = StateSite.random(
            physical_dimension = 9,
            left_dimension = leftmost_dimension,
            right_dimension = middle_dimension,
        )
        right_site = StateSite.random(
            physical_dimension = 9,
            left_dimension = middle_dimension,
            right_dimension = rightmost_dimension,
        )
        if direction == Direction.right:
            new_right_site, new_left_site = right_site.normalizeAndDenormalize(direction,left_site)

        else:
            new_left_site, new_right_site = left_site.normalizeAndDenormalize(direction,right_site)
        self.assertEqual(new_left_site.dimensions(),left_site.dimensions())
        self.assertEqual(new_right_site.dimensions(),right_site.dimensions())
        self.assertAllClose(
            tensordot(new_left_site.data,new_right_site.data,(StateSite.right_index,StateSite.left_index)),
            tensordot(left_site.data,right_site.data,(StateSite.right_index,StateSite.left_index)),
        )
        if direction == Direction.right:
            self.assertNormalized(new_right_site.data,StateSite.left_index)
        else:
            self.assertNormalized(new_left_site.data,StateSite.right_index)
    # }}}

    @with_checker
    def test_random_left_normalized(self, # {{{
        physical_dimension = irange(3,9),
        left_dimension = irange(3,9),
        right_dimension = irange(3,9)
    ):
        state_site = StateSite.random(
            normalization = Normalization.left,
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
        )
        self.assertNormalized(state_site.data,StateSite.right_index)
    # }}}

    @with_checker
    def test_random_middle_normalized(self, # {{{
        physical_dimension = irange(3,9),
        left_dimension = irange(3,9),
        right_dimension = irange(3,9)
    ):
        state_site = StateSite.random(
            normalization = Normalization.middle,
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
        )
        self.assertAlmostEqual(norm(state_site.data),1)
    # }}}

    @with_checker
    def test_random_right_normalized(self, # {{{
        physical_dimension = irange(3,9),
        left_dimension = irange(3,9),
        right_dimension = irange(3,9)
    ):
        state_site = StateSite.random(
            normalization = Normalization.right,
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
        )
        self.assertNormalized(state_site.data,StateSite.left_index)
    # }}}

    @with_checker
    def test_simpleObservation(self,physical_dimension=irange(1,10)): # {{{
        observation = randint(0,physical_dimension-1)
        state_site = StateSite.simpleObservation(physical_dimension,observation)
        shape = [1]*StateSite.number_of_dimensions
        shape[StateSite.indexForName("physical")] = physical_dimension
        correct_data = zeros(physical_dimension)
        correct_data[observation] = 1
        correct_data = correct_data.reshape(shape)
        self.assertAllClose(state_site.data,correct_data)
    # }}}
# }}}
