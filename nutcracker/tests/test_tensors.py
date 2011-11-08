#@+leo-ver=5-thin
#@+node:gcross.20111107131531.1333: * @file test_tensors.py
#@+<< Imports >>
#@+node:gcross.20111107131531.1334: ** << Imports >>
from numpy import identity, tensordot

from paycheck import *

from nutcracker.tensors import *
from nutcracker.utils import *

from nutcracker.tests import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111107131531.1347: ** Tests
#@+node:gcross.20111107131531.1348: *3* TestLeftExpectationBoundary
class TestLeftExpectationBoundary(TestCase):
    #@+others
    #@+node:gcross.20111107131531.1349: *4* absorb
    @with_checker
    def test_absorb(self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_operator_dimension=irange(1,4),
        right_operator_dimension=irange(1,4),
    ):
        L = LeftExpectationBoundary(
            state_dimension = left_state_dimension,
            operator_dimension = left_operator_dimension,
            randomize = True
        )
        O = OperatorSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_operator_dimension,
            right_dimension = right_operator_dimension,
        )
        S = StateSite(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
            randomize = True
        )
        self.assertAllClose(
            L.absorb(O,S).data,
            self.test_absorb.contract(L.data,O.formDenseTensor(),S.data,S.data.conj())
        )

    test_absorb.contract = formContractor(
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
         }[name] for name in LeftExpectationBoundary._dimensions
        ]
    )
    #@-others
#@+node:gcross.20111107131531.3583: *3* TestRightExpectationBoundary
class TestRightExpectationBoundary(TestCase):
    #@+others
    #@+node:gcross.20111107131531.3584: *4* absorb
    @with_checker
    def test_absorb(self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_operator_dimension=irange(1,4),
        right_operator_dimension=irange(1,4),
    ):
        R = RightExpectationBoundary(
            state_dimension = right_state_dimension,
            operator_dimension = right_operator_dimension,
            randomize = True
        )
        O = OperatorSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_operator_dimension,
            right_dimension = right_operator_dimension,
        )
        S = StateSite(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
            randomize = True
        )
        self.assertAllClose(
            R.absorb(O,S).data,
            self.test_absorb.contract(R.data,O.formDenseTensor(),S.data,S.data.conj())
        )

    test_absorb.contract = formContractor(
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
         }[name] for name in RightExpectationBoundary._dimensions
        ]
    )
    #@-others
#@+node:gcross.20111107131531.3591: *3* TestStateSite
class TestStateSite(TestCase):
    #@+others
    #@+node:gcross.20111108100704.1372: *4* formNormalizedOverlapSites
    @with_checker
    def test_formNormalizedOverlapSites(self,
        left_physical_dimension = irange(2,4),
        right_physical_dimension = irange(2,4),
        leftmost_bandwidth_dimension = irange(2,4),
        middle_bandwidth_dimension = irange(2,4),
        rightmost_bandwidth_dimension = irange(2,4),
    ):
        unnormalized_left_state_site = StateSite(
            physical_dimension = left_physical_dimension,
            left_dimension = leftmost_bandwidth_dimension,
            right_dimension = middle_bandwidth_dimension,
            randomize = True
        )
        right_normalized_right_state_site = StateSite(
            physical_dimension = right_physical_dimension,
            left_dimension = middle_bandwidth_dimension,
            right_dimension = rightmost_bandwidth_dimension,
            randomize = True
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
    #@+node:gcross.20111107131531.5850: *4* formOverlapSite
    @with_checker
    def test_formOverlapSite(self,
        physical_dimension = irange(1,5),
        left_dimension = irange(1,5),
        right_dimension = irange(1,5)
    ):
        state_site = StateSite(
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
            randomize = True
        )
        self.assertAllClose(
            state_site.formOverlapSite().data,
            state_site.data.conj().transpose(2,0,1)
        )
    #@+node:gcross.20111107131531.3593: *4* normalizeAndDenormalize
    @with_checker
    def test_normalizeAndDenormalize(self,
        direction=irange(0,1),
        leftmost_dimension=irange(3,9),
        middle_dimension=irange(3,9),
        rightmost_dimension=irange(3,9),
    ):
        left_site = StateSite(
            physical_dimension = 9,
            left_dimension = leftmost_dimension,
            right_dimension = middle_dimension,
            randomize = True
        )
        right_site = StateSite(
            physical_dimension = 9,
            left_dimension = middle_dimension,
            right_dimension = rightmost_dimension,
            randomize = True
        )
        if direction == LEFT:
            new_right_site, new_left_site = right_site.normalizeAndDenormalize(left_site,direction)

        else:
            new_left_site, new_right_site = left_site.normalizeAndDenormalize(right_site,direction)
        self.assertEqual(new_left_site.data.shape,left_site.data.shape)
        self.assertEqual(new_right_site.data.shape,right_site.data.shape)
        self.assertAllClose(
            tensordot(new_left_site.data,new_right_site.data,(StateSite.right_index,StateSite.left_index)),
            tensordot(left_site.data,right_site.data,(StateSite.right_index,StateSite.left_index)),
        )
        if direction == LEFT:
            self.assertNormalized(new_right_site.data,StateSite.left_index)
        else:
            self.assertNormalized(new_left_site.data,StateSite.right_index)
    #@-others
#@-others
#@-leo
