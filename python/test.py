#@+leo-ver=5-thin
#@+node:gcross.20110906104131.2941: * @file test.py
#@+<< Import needed modules >>
#@+node:gcross.20110906104131.2942: ** << Import needed modules >>
import itertools
from numpy import set_printoptions
import os
import unittest

from Nutcracker import *
#@-<< Import needed modules >>

#@+others
#@+node:gcross.20110906130654.2877: ** Tests
#@+others
#@+node:gcross.20110906155043.2996: *3* Matrix
class MatrixTests(unittest.TestCase):
    #@+others
    #@+node:gcross.20110906155043.2998: *4* new
    def test_new(self):
        m = Matrix([[1,2],[3,4]])
        self.assertEqual(len(m),2)
        self.assertEqual([1,2,3,4],list(m))
    #@+node:gcross.20110906155043.4810: *4* newDiagonal
    def test_newDiagonal(self):
        m = Matrix([1,2])
        self.assertEqual(len(m),2)
        self.assertEqual([1,0,0,2],list(m))
    #@+node:gcross.20110906155043.4811: *4* constants
    def test_constants(self):
        self.assertEqual([1,0,0,1],list(Matrix.pauli_I))
        self.assertEqual([0,1,1,0],list(Matrix.pauli_X))
        self.assertEqual([0,-1j,1j,0],list(Matrix.pauli_Y))
        self.assertEqual([1,0,0,-1],list(Matrix.pauli_Z))
    #@-others
#@+node:gcross.20110906130654.2947: *3* OperatorBuilder
class OperatorBuilderTests(unittest.TestCase):
    #@+others
    #@+node:gcross.20110906155043.4822: *4* addProduct
    def test_addProduct(self):
        for number_of_sites in range(2,6):
            builder = OperatorBuilder(number_of_sites,2)
            for site_number in range(number_of_sites):
                builder.addProductTerm(
                    [Matrix.pauli_I]*site_number +
                    [Matrix([0,site_number])] +
                    [Matrix.pauli_I]*(number_of_sites-site_number-1)
                )
            operator = builder.compile()
            for site_number in range(number_of_sites):
                self.assertEqual(
                    StateBuilder(number_of_sites,2)
                        .addProductTerm(
                            [Vector.qubit_up]*site_number +
                            [Vector.qubit_down] +
                            [Vector.qubit_up]*(number_of_sites-site_number-1)
                         )
                        .compile()
                        * operator
                   ,site_number
                )
    #@+node:gcross.20110906130654.2948: *4* new
    def test_new(self):
        self.assertEqual([1,2,3],list(OperatorBuilder([1,2,3])))
    #@+node:gcross.20110906130654.2949: *4* newSimple
    def test_newSimple(self):
        self.assertEqual([2]*3,list(OperatorBuilder(3,2)))
    #@-others
#@+node:gcross.20110906130654.2922: *3* StateBuilder
class StateBuilderTests(unittest.TestCase):
    #@+others
    #@+node:gcross.20110906130654.2924: *4* new
    def test_new(self):
        self.assertEqual([1,2,3],list(StateBuilder([1,2,3])))
    #@+node:gcross.20110906130654.2931: *4* newSimple
    def test_newSimple(self):
        self.assertEqual([2]*3,list(StateBuilder(3,2)))
    #@+node:gcross.20110906155043.2975: *4* orthogonal basis
    def test_orthogonal_basis(self):
        for number_of_sites in range(2,6):
            states = [
                StateBuilder(number_of_sites,2)
                    .addProductTerm([Vector.qubit_up]*site_number + [Vector.qubit_down] + [Vector.qubit_up]*(number_of_sites-site_number-1))
                    .compile()
                for site_number in range(0,number_of_sites)
            ]
            for (i,j) in itertools.product(states,states):
                if i is j:
                    self.assertAlmostEqual(1,i * j)
                else:
                    self.assertAlmostEqual(0,i * j)
    #@-others
#@+node:gcross.20110906130654.2876: *3* Vector
class VectorTests(unittest.TestCase):
    #@+others
    #@+node:gcross.20110906130654.2890: *4* __iter__
    def test___iter__(self):
        self.assertEqual([1,2,3],list(Vector([1,2,3])))
    #@+node:gcross.20110906130654.2892: *4* __reversed__
    def test___reversed__(self):
        self.assertEqual([3,2,1],list(reversed(Vector([1,2,3]))))
    #@+node:gcross.20110906130654.2882: *4* add
    def test_add(self):
        v = Vector([1,2,3]) + Vector([4j,5j,6j])
        self.assertEqual(len(v),3)
        self.assertEqual(v[0],1+4j)
        self.assertEqual(v[1],2+5j)
        self.assertEqual(v[2],3+6j)
    #@+node:gcross.20110906130654.2884: *4* multiply
    def test_multiply(self):
        self.assertEqual([1+1j,2+2j,3+3j],list(Vector([1,2,3]) * (1+1j)))
        self.assertEqual([1+1j,2+2j,3+3j],list((1+1j) * Vector([1,2,3])))
    #@+node:gcross.20110906130654.2878: *4* new
    def test_new(self):
        v = Vector([1,2,3])
        self.assertEqual(len(v),3)
        self.assertEqual(v[0],1)
        self.assertEqual(v[1],2)
        self.assertEqual(v[2],3)
    #@+node:gcross.20110906130654.2880: *4* newBasis
    def test_newBasis(self):
        v = Vector(3,1)
        self.assertEqual(len(v),3)
        self.assertEqual(v[0],0)
        self.assertEqual(v[1],1)
        self.assertEqual(v[2],0)
    #@+node:gcross.20110906130654.2899: *4* constants
    def test_constants(self):
        self.assertEqual([1,0],list(Vector.qubit_up))
        self.assertEqual([0,1],list(Vector.qubit_down))
    #@-others
#@-others

tests = [
    MatrixTests,
    OperatorBuilderTests,
    StateBuilderTests,
    VectorTests,
]
#@-others

set_printoptions(linewidth=132)

#@+<< Runner >>
#@+node:gcross.20110906104131.3033: ** << Runner >>
unittest.TextTestRunner(verbosity=2).run(unittest.TestSuite(map(unittest.defaultTestLoader.loadTestsFromTestCase, tests)))
#@-<< Runner >>
#@-leo
