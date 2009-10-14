#@+leo-ver=4-thin
#@+node:gcross.20091012202301.1605:@thin trial.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091013113202.1450:<< Import needed modules >>
from cPickle import dumps, loads
from foolscap.api import Referenceable, Copyable, RemoteCopy
from lrucache import LRUCache
from twisted.internet.defer import Deferred, DeferredList, inlineCallbacks, returnValue
from twisted.internet.threads import deferToThread
from uuid import uuid4

from simulation import run_simulation
from utils import convert_old_state_tensors_to_orthogonal_state_information
#@-node:gcross.20091013113202.1450:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091012202301.1606:Classes
#@+node:gcross.20091012202301.1610:StateServer
class StateServer(Referenceable):
    #@    @+others
    #@+node:gcross.20091012202301.1616:__slots__
    __slots__ = ["state_site_tensors","orthogonal_state_information_pickled"]
    #@-node:gcross.20091012202301.1616:__slots__
    #@+node:gcross.20091012202301.1617:__init__
    def __init__(self,state_site_tensors):
        self.state_site_tensors = state_site_tensors
    #@-node:gcross.20091012202301.1617:__init__
    #@+node:gcross.20091012202301.1618:remote_fetch
    def remote_fetch(self):
        try:
            return self.orthogonal_state_information_pickled
        except AttributeError:
            self.orthogonal_state_information_pickled = dumps(convert_old_state_tensors_to_orthogonal_state_information(self.state_site_tensors),2)
            return self.orthogonal_state_information_pickled
    #@-node:gcross.20091012202301.1618:remote_fetch
    #@+node:gcross.20091013163748.4189:callRemote
    def callRemote(self,method_name,*args,**keywords):
        return getattr(self,"remote_"+method_name)(*args,**keywords)
    #@-node:gcross.20091013163748.4189:callRemote
    #@-others
#@-node:gcross.20091012202301.1610:StateServer
#@+node:gcross.20091012202301.1608:StateFetcher
class StateFetcher(Copyable,RemoteCopy):
    #@    @+others
    #@+node:gcross.20091012202301.1634:__init__
    def __init__(self,uuid=None,remote_server=None):
        self.uuid = str(uuid)
        self.remote_server = remote_server
    #@-node:gcross.20091012202301.1634:__init__
    #@+node:gcross.20091012202301.1612:(class fields)
    typeToCopy = copytype = "StateFetcher"

    cache = LRUCache(10)
    #@-node:gcross.20091012202301.1612:(class fields)
    #@+node:gcross.20091013163748.4184:setCopyState
    def setCopyableState(self, state):
        self.__dict__ = state
    #@-node:gcross.20091013163748.4184:setCopyState
    #@+node:gcross.20091012202301.1615:_received
    def _received(self,state_information):
        self.cache[self.uuid] = state_information
        return state_information
    #@-node:gcross.20091012202301.1615:_received
    #@+node:gcross.20091012202301.1614:fetch
    @inlineCallbacks
    def fetch(self):
        if self.uuid in self.cache:
            returnValue(self.cache[uuid])
        else:
            orthogonal_state_information_pickled = yield self.remote_server.callRemote("fetch")
            returnValue(loads(orthogonal_state_information_pickled))
    #@-node:gcross.20091012202301.1614:fetch
    #@-others
#@-node:gcross.20091012202301.1608:StateFetcher
#@+node:gcross.20091012202301.1633:TrialResult
class TrialResult(Copyable,RemoteCopy):
    #@    @+others
    #@+node:gcross.20091013134215.1462:(copy type)
    typeToCopy = copytype = "TrialResult"
    #@-node:gcross.20091013134215.1462:(copy type)
    #@+node:gcross.20091013134215.1448:__init__
    def __init__(self,energy=None,state_site_tensors=None):
        if energy is not None:
            self.energy = float(energy)
            self.state_fetcher = StateFetcher(uuid4(),StateServer(state_site_tensors))
    #@-node:gcross.20091013134215.1448:__init__
    #@+node:gcross.20091013163748.4186:setCopyState
    def setCopyableState(self, state):
        self.__dict__ = state
    #@-node:gcross.20091013163748.4186:setCopyState
    #@-others
#@-node:gcross.20091012202301.1633:TrialResult
#@+node:gcross.20091012202301.1607:TrialTask
class TrialTask(Copyable,RemoteCopy):
    #@    @+others
    #@+node:gcross.20091013163748.1470:(copy type)
    typeToCopy = copytype = "TrialTask"
    #@-node:gcross.20091013163748.1470:(copy type)
    #@+node:gcross.20091012202301.1619:__init__
    def __init__(self,
      number_of_sites=None,
      physical_dimension=None,
      starting_bandwidth_dimension=None,
      operator_factory=None,
      old_state_fetchers=[],
      ):
        self.number_of_sites = number_of_sites
        self.physical_dimension = physical_dimension
        self.starting_bandwidth_dimension = starting_bandwidth_dimension
        self.operator_factory = operator_factory
        self.old_state_fetchers = old_state_fetchers
    #@-node:gcross.20091012202301.1619:__init__
    #@+node:gcross.20091013113202.1448:__call__
    @inlineCallbacks
    def __call__(self):
        orthogonal_state_information_list_results = yield DeferredList(map(lambda x: x.fetch(), self.old_state_fetchers))
        if orthogonal_state_information_list_results:
            orthogonal_state_information_list = zip(*orthogonal_state_information_list_results)[1]
        else:
            orthogonal_state_information_list = []

        def sweep_callback(bandwidth_dimension,sweep_number,energy):
            level_number = len(orthogonal_state_information_list)+1
            print ("Level number {level_number}, Bandwidth dimension {bandwidth_dimension}, Sweep number {sweep_number}: {energy}".format(**vars()))

        result = yield deferToThread(run_simulation,
            self.number_of_sites,
            self.physical_dimension,
            self.starting_bandwidth_dimension,
            self.operator_factory(),
            orthogonal_state_information_list,
            sweep_callback,
        )

        returnValue(TrialResult(*result))
    #@-node:gcross.20091013113202.1448:__call__
    #@+node:gcross.20091013163748.4188:setCopyState
    def setCopyableState(self, state):
        self.__dict__ = state
    #@-node:gcross.20091013163748.4188:setCopyState
    #@-others
#@-node:gcross.20091012202301.1607:TrialTask
#@-node:gcross.20091012202301.1606:Classes
#@-others
#@-node:gcross.20091012202301.1605:@thin trial.py
#@-leo
