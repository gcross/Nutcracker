#@+leo-ver=4-thin
#@+node:gcross.20091013163748.4164:@thin controller.py
#@@language Python

import warnings
warnings.simplefilter('ignore', DeprecationWarning)

#@<< Import needed modules >>
#@+node:gcross.20091013163748.4183:<< Import needed modules >>
from foolscap.api import Tub, Referenceable, Copyable, RemoteCopy
import subprocess
from twisted.internet import reactor
from twisted.internet.defer import Deferred, inlineCallbacks, returnValue
#@-node:gcross.20091013163748.4183:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091013163748.4171:Functions
#@+node:gcross.20091013163748.4172:becomeControllerAndThenCall
def becomeControllerAndThenCall(function_to_call):
    tub = Tub()
    tub.listenOn("tcp:62343")
    tub.startService()
    controller = Controller()
    @inlineCallbacks
    def initialize(_):
        global furl
        furl = yield tub.registerReference(controller)
        yield function_to_call(controller,furl)
    tub.setLocationAutomatically().addCallback(initialize)
    reactor.run()
#@-node:gcross.20091013163748.4172:becomeControllerAndThenCall
#@+node:gcross.20091013184459.1472:spawnWorkerProcess
def spawnWorkerProcess():
    global furl
    subprocess.Popen(["python","worker.py",furl])
#@-node:gcross.20091013184459.1472:spawnWorkerProcess
#@-node:gcross.20091013163748.4171:Functions
#@+node:gcross.20091013163748.4165:Classes
#@+node:gcross.20091013163748.4166:Controller
class Controller(Referenceable):
    #@    @+others
    #@+node:gcross.20091013163748.4176:__slots__
    __slots__ = ["workers","tasks"]
    #@-node:gcross.20091013163748.4176:__slots__
    #@+node:gcross.20091013163748.4167:__init__
    def __init__(self):
        self.workers = []
        self.tasks = []
    #@-node:gcross.20091013163748.4167:__init__
    #@+node:gcross.20091013163748.4168:remote_add_worker
    def remote_add_worker(self,worker):
        self.workers.append(worker)
        self.distribute_tasks()
    #@-node:gcross.20091013163748.4168:remote_add_worker
    #@+node:gcross.20091013184459.4169:remote_ping
    def remote_ping(self):
        pass
    #@-node:gcross.20091013184459.4169:remote_ping
    #@+node:gcross.20091013163748.4170:add_task
    def add_task(self,task):
        d = Deferred()
        self.tasks.append((task,d))
        self.distribute_tasks()
        return d
    #@-node:gcross.20091013163748.4170:add_task
    #@+node:gcross.20091013163748.4169:distribute_tasks
    def distribute_tasks(self):
        while len(self.workers) > 0 and len(self.tasks) > 0:
            worker = self.workers.pop()
            (task,deferred) = self.tasks.pop()
            def return_worker_to_pool(result):
                self.workers.append(worker)
                self.distribute_tasks()
                return result
            worker.callRemote("run",task).addBoth(return_worker_to_pool).chainDeferred(deferred)
    #@-node:gcross.20091013163748.4169:distribute_tasks
    #@-others
#@-node:gcross.20091013163748.4166:Controller
#@-node:gcross.20091013163748.4165:Classes
#@-others

__all__ = ["becomeControllerAndThenCall","spawnWorkerProcess"]
#@-node:gcross.20091013163748.4164:@thin controller.py
#@-leo
