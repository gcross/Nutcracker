# Imports {{{
from .tensors import *
# }}} Imports

# Tensors {{{

class LeftExpectationBoundary(Tensor): # {{{
    index_names = ["operator","state_conjugate","state"]

    def absorb(self,operator,state):
        return self.absorb_contractor(state.conj(),operator,state)

    def contractWithRightBoundary(self,right_boundary):
        return self.contractWithRightBoundary_contractor(self,right_boundary).extractScalar()
# }}}

class RightExpectationBoundary(Tensor): # {{{
    index_names = ["operator","state","state_conjugate"]

    def absorb(self,operator,state):
        return self.absorb_contractor(state.conj(),operator,state)

    def contractWithLeftBoundary(self,left_boundary):
        return self.contractWithLeftBoundary_contractor(right_boundary,self).extractScalar()
# }}}

class StateSite(Tensor): # {{{
    index_names = ["physical","left","right"]

    def conj(self):
        return StateConjugateSite(self.data.conj())
# }}}

class StateConjugateSite(Tensor): # {{{
    index_names = ["physical","left","right"]
# }}}

class OperatorSite(Tensor): # {{{
    index_names = ["left","right","physical_conjugate","physical"]
# }}}

class IterationStage1(Tensor): # {{{
    index_names = ["physical_conjugate","state_conjugate","operator","physical","state"]

    def __init__(self,data,right_boundary):
        Tensor.__init__(self,data)
        self.right_boundary = right_boundary
        self.state_site_shape = StateSite.constructShape(
            physical=self.physical_dimension,
            left=self.state_dimension,
            right=right_boundary.state_dimension
        )

    @classmethod
    def form(cls,left_boundary,operator_site):
        return cls(cls.form_contractor(left_boundary,operator_site))

    def matvec(self,v):
        return self.matvec_contract(
            StateSite(NDArrayData(v.reshape(self.state_site_shape))),
            self.right_boundary
        ).data.asArray().ravel()
# }}}

# }}} Tensors

# Register joins and groupings {{{

registerLegalJoin(LeftExpectationBoundary,"operator",OperatorSite,"left")
registerLegalJoin(LeftExpectationBoundary,"state",StateSite,"left")
registerLegalJoin(LeftExpectationBoundary,"state_conjugate",StateConjugateSite,"left")

registerLegalGrouping(LeftExpectationBoundary,"operator",(OperatorSite,"right"))
registerLegalGrouping(LeftExpectationBoundary,"state",(StateSite,"right"))
registerLegalGrouping(LeftExpectationBoundary,"state_conjugate",(StateConjugateSite,"right"))

registerLegalJoin(RightExpectationBoundary,"operator",OperatorSite,"right")
registerLegalJoin(RightExpectationBoundary,"state",StateSite,"right")
registerLegalJoin(RightExpectationBoundary,"state_conjugate",StateConjugateSite,"right")

registerLegalGrouping(RightExpectationBoundary,"operator",(OperatorSite,"left"))
registerLegalGrouping(RightExpectationBoundary,"state",(StateSite,"left"))
registerLegalGrouping(RightExpectationBoundary,"state_conjugate",(StateConjugateSite,"left"))

registerLegalJoin(LeftExpectationBoundary,"operator",RightExpectationBoundary,"operator")
registerLegalJoin(LeftExpectationBoundary,"state",RightExpectationBoundary,"state")
registerLegalJoin(LeftExpectationBoundary,"state_conjugate",RightExpectationBoundary,"state_conjugate")

registerLegalJoin(StateSite,"physical",OperatorSite,"physical")
registerLegalJoin(StateConjugateSite,"physical",OperatorSite,"physical_conjugate")

registerLegalJoin(IterationStage1,"state",StateSite,"left")
registerLegalJoin(IterationStage1,"state_conjugate",StateConjugateSite,"left")
registerLegalJoin(IterationStage1,"operator",RightExpectationBoundary,"operator")
registerLegalJoin(IterationStage1,"physical",StateSite,"physical")
registerLegalJoin(IterationStage1,"physical_conjugate",StateConjugateSite,"physical")

registerLegalGrouping(IterationStage1,"state",(LeftExpectationBoundary,"state"))
registerLegalGrouping(IterationStage1,"state_conjugate",(LeftExpectationBoundary,"state_conjugate"))
registerLegalGrouping(IterationStage1,"operator",(OperatorSite,"right"))
registerLegalGrouping(IterationStage1,"physical",(OperatorSite,"physical"))
registerLegalGrouping(IterationStage1,"physical_conjugate",(OperatorSite,"physical_conjugate"))

# }}}

# Construct contractors {{{

LeftExpectationBoundary.absorb_contractor = makeTensorContractor( # {{{
    [LeftExpectationBoundary,StateConjugateSite,OperatorSite,StateSite], [LeftExpectationBoundary],
    [
        Join(0,"operator",2,"left"),
        Join(0,"state",3,"left"),
        Join(0,"state_conjugate",1,"left"),
        Join(1,"physical",2,"physical_conjugate"),
        Join(3,"physical",2,"physical"),
    ],
    {
        "state_conjugate": [(1,"right")],
        "operator": [(2,"right")],
        "state": [(3,"right")],
    }
) # }}}

RightExpectationBoundary.absorb_contractor = makeTensorContractor( # {{{
    [RightExpectationBoundary,StateConjugateSite,OperatorSite,StateSite], [RightExpectationBoundary],
    [
        Join(0,"state_conjugate",1,"right"),
        Join(3,"physical",2,"physical"),
        Join(0,"operator",2,"right"),
        Join(0,"state",3,"right"),
        Join(1,"physical",2,"physical_conjugate"),
    ],
    {
        "state_conjugate": [(1,"left")],
        "operator": [(2,"left")],
        "state": [(3,"left")],
    }
) # }}}

LeftExpectationBoundary.contractWithRightBoundary_contractor = staticmethod(makeTensorContractor # {{{
    [LeftExpectationBoundary,RightExpectationBoundary],
)) # }}}

IterationStage1.form_contractor = staticmethod(makeTensorContractor( # {{{
    [LeftExpectationBoundary,OperatorSite],IterationStage1,
    [
        Join(0,"operator",1,"left"),
    ],
    {
        "state": [(0,"state")],
        "state_conjugate": [(0,"state_conjugate")],
        "physical": [(1,"physical")],
        "physical_conjugate": [(1,"physical_conjugate")],
        "operator": [(1,"right")],
    }
)) # }}}

IterationStage1.matvec_contractor = makeTensorContractor( # {{{
    [IterationStage1,StateSite,RightExpectationBoundary],StateConjugateSite,
    [
        Join(0,"physical",1,"physical"),
        Join(0,"state",1,"left"),
        Join(0,"operator",2,"operator"),
        Join(1,"right",2,"state"),
    ],
    {
        "physical": [(0,"physical_conjugate")],
        "left": [(0,"state")],
        "right": [(2,"state")],
    }
) # }}}

# }}}
