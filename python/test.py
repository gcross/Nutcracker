#@+leo-ver=5-thin
#@+node:gcross.20110906104131.2941: * @file test.py
#@+<< Import needed modules >>
#@+node:gcross.20110906104131.2942: ** << Import needed modules >>
from numpy import set_printoptions
import os
import unittest

from Nutcracker import *
#@-<< Import needed modules >>

#@+others
#@+node:gcross.20110906130654.2877: ** Tests
#@+others
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
        v = Vector([1,2,3]) * (1+1j)
        self.assertEqual(len(v),3)
        self.assertEqual(v[0],1+1j)
        self.assertEqual(v[1],2+2j)
        self.assertEqual(v[2],3+3j)
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
    #@-others
#@-others

tests = [
    VectorTests,
]
#@-others

set_printoptions(linewidth=132)

#@+<< Runner >>
#@+node:gcross.20110906104131.3033: ** << Runner >>
unittest.TextTestRunner(verbosity=2).run(unittest.TestSuite(map(unittest.defaultTestLoader.loadTestsFromTestCase, tests)))
#@-<< Runner >>
#@-leo
