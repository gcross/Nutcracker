#@+leo-ver=5-thin
#@+node:gcross.20111107131531.3618: * @file test_environment.py
#@+<< Imports >>
#@+node:gcross.20111107131531.3619: ** << Imports >>
from random import randint

from nutcracker.environment import *
from nutcracker.utils import *

from nutcracker.tests import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111107131531.3624: ** Functions
#@+node:gcross.20111107131531.3622: *3* randomEnvironment
def randomEnvironment():
    return Environment.random(
        randint(1,5),
        randint(1,5),
        randint(1,5),
        randint(1,5),
        randint(1,5)
    )
#@+node:gcross.20111107131531.3620: ** Tests
#@+node:gcross.20111107131531.3621: *3* Environment
class TestEnvironment(TestCase):
    #@+others
    #@+node:gcross.20111107131531.3623: *4* computeExpectation
    def test_computeExpectation(self):
        environment = randomEnvironment()
        self.assertAlmostEqual(
            environment.computeExpectation(),
            self.test_computeExpectation.contract(
                environment.left_environment.data,
                environment.right_environment.data,
                environment.state_site.data,
                environment.state_site.data.conj(),
                environment.operator_site.formDenseTensor(),
            )
        )

    test_computeExpectation.contract = formContractor(
        ['L','R','S','S*','O',],
        [(('L',1),('S*',1))
        ,(('L',0),('O',0))
        ,(('L',2),('S',1))
        ,(('R',2),('S*',2))
        ,(('R',0),('O',1))
        ,(('R',1),('S',2))
        ,(('O',2),('S*',0))
        ,(('O',3),('S',0))
        ],
        []
    )
    #@-others
#@-others
#@-leo
