#@+leo-ver=5-thin
#@+node:gcross.20111107131531.1346: * @file __init__.py
#@+<< Imports >>
#@+node:gcross.20111107131531.1351: ** << Imports >>
from numpy import all, allclose, array
from numpy.linalg import norm
import unittest
#@-<< Imports >>

#@+others
#@+node:gcross.20111107123726.3152: ** Classes
#@+node:gcross.20111107123726.3153: *3* TestCase
class TestCase(unittest.TestCase):
    #@+others
    #@+node:gcross.20111107123726.3154: *4* assertAllClose
    def assertAllClose(self,v1,v2):
        v1 = array(v1)
        v2 = array(v2)
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(allclose(v1,v2))
    #@+node:gcross.20111107123726.3155: *4* assertAllEqual
    def assertAllEqual(self,v1,v2):
        v1 = array(v1)
        v2 = array(v2)
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(all(v1 == v2))
    #@+node:gcross.20111107123726.3156: *4* assertVanishing
    def assertVanishing(self,v):
        self.assertAlmostEqual(norm(v),0)
    #@-others
#@-others
#@-leo