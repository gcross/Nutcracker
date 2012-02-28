# Imports {{{
from paycheck import *

from ..data import *
from ..tensors import *
from ..utils import *

from . import *
# }}}

# Tests {{{

class test_makeTensorContractor(TestCase): # {{{
  # Tensor classes {{{
    class L(Tensor):
        index_names = ["left","middle"]
    class R(Tensor):
        index_names = ["middle","right"]
    registerLegalJoin(L,"middle",R,"middle")

    class LR(Tensor):
        index_names = ["left","right"]
    registerLegalGrouping(LR,"left",(L,"left"))
    registerLegalGrouping(LR,"left",(R,"right"))
    registerLegalGrouping(LR,"right",(L,"left"))
    registerLegalGrouping(LR,"right",(R,"right"))

    class LR_as_matrix(Tensor):
        index_names = ["left","right"]
    registerLegalGrouping(LR_as_matrix,"left",(L,"left"))
    registerLegalGrouping(LR_as_matrix,"right",(R,"right"))

    class LR_as_vector(Tensor):
        index_names = ["leftright"]
    registerLegalGrouping(LR_as_vector,"leftright",(L,"left"),(R,"right"))

  # }}}
  # Tests {{{
    @with_checker
    def test_matrix_contractor(self,m=irange(1,10),n=irange(1,10),k=irange(1,10)): # {{{
        contractTensors = makeTensorContractor(
            [self.L,self.R],self.LR_as_matrix,
            [Join(0,"middle",1,"middle")],
            [
                [(0,"left")],
                [(1,"right")],
            ]
        )
        l = self.L(NDArrayData.newRandom((m,k)))
        r = self.R(NDArrayData.newRandom((k,n)))
        lr = contractTensors(l,r)
        self.assertTrue(isinstance(lr,self.LR_as_matrix))
        contractData = makeDataContractor(
            [Join(0,1,1,0)],
            [
                [(0,0)],
                [(1,1)],
            ]
        )
        self.assertDataAlmostEqual(lr.data,contractData(l.data,r.data))
    # }}}
    @with_checker
    def test_vector_contractor(self,m=irange(1,10),n=irange(1,10),k=irange(1,10)): # {{{
        contractTensors = makeTensorContractor(
            [self.L,self.R],self.LR_as_vector,
            [Join(0,"middle",1,"middle")],
            [
                [(0,"left"),(1,"right")],
            ]
        )
        l = self.L(NDArrayData.newRandom((m,k)))
        r = self.R(NDArrayData.newRandom((k,n)))
        lr = contractTensors(l,r)
        self.assertTrue(isinstance(lr,self.LR_as_vector))
        contractData = makeDataContractor(
            [Join(0,1,1,0)],
            [
                [(0,0),(1,1)],
            ]
        )
        self.assertDataAlmostEqual(lr.data,contractData(l.data,r.data))
    # }}}
    def test_bad_join(self): # {{{
        try:
            makeTensorContractor(
                [self.L,self.R],self.LR,
                [Join(0,"left",1,"middle")],
                [
                    [(0,"middle")],
                    [(1,"right")],
                ]
            )
        except IllegalJoinError as e:
            self.assertEqual(e.left_tensor_class,self.L)
            self.assertEqual(e.left_index_name,"left")
            self.assertEqual(e.right_tensor_class,self.R)
            self.assertEqual(e.right_index_name,"middle")
    # }}}
    def test_bad_grouping_1(self): # {{{
        try:
            makeTensorContractor(
                [self.L,self.R],self.LR_as_matrix,
                [Join(0,"middle",1,"middle")],
                [
                    [(1,"right")],
                    [(0,"left")],
                ]
            )
        except IllegalGroupingError as e:
            self.assertEqual(e.tensor_class,self.LR_as_matrix)
            self.assertEqual(e.index_name,"left")
            self.assertEqual(e.grouping,[(self.R,"right")])
    # }}}
    def test_bad_grouping_2(self): # {{{
        try:
            makeTensorContractor(
                [self.L,self.R],self.LR_as_matrix,
                [Join(0,"middle",1,"middle")],
                [
                    [(0,"left"),(1,"right")],
                    [],
                ]
            )
        except IllegalGroupingError as e:
            self.assertEqual(e.tensor_class,self.LR_as_matrix)
            self.assertEqual(e.index_name,"left")
            self.assertEqual(e.grouping,[(self.L,"left"),(self.R,"right")])
    # }}}
    def test_bad_grouping_3(self): # {{{
        try:
            makeTensorContractor(
                [self.L,self.R],self.LR_as_vector,
                [Join(0,"middle",1,"middle")],
                [
                    [(1,"right"),(0,"left")],
                ]
            )
        except IllegalGroupingError as e:
            self.assertEqual(e.tensor_class,self.LR_as_vector)
            self.assertEqual(e.index_name,"leftright")
            self.assertEqual(e.grouping,[(self.R,"right"),(self.L,"left")])
    # }}}
  # }}}
# }}}

# }}}
