#@+leo-ver=4-thin
#@+node:gcross.20091013163748.4164:@thin grid.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091013163748.4183:<< Import needed modules >>
from foolscap.api import Tub, Referenceable, Copyable, RemoteCopy
from twisted.internet.defer import Deferred, inlineCallbacks, returnValue
#@-node:gcross.20091013163748.4183:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091013163748.4171:Functions
#@+node:gcross.20091013163748.4172:startController
@inlineCallbacks
def startController():
    tub = Tub()
    tub.listenOn("tcp:62343")
    tub.startService()
    controller = Controller()
    yield tub.setLocationAutomatically()
    furl = yield tub.registerReference(controller)
    returnValue((controller,furl))
#@-node:gcross.20091013163748.4172:startController
#@+node:gcross.20091013163748.4173:startWorker
@inlineCallbacks
def startWorker(furl):
    tub = Tub()
    tub.startService()
    controller = yield tub.getReference(furl)
    yield controller.callRemote("add_worker",Worker())
#@-node:gcross.20091013163748.4173:startWorker
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
                return result
            worker.callRemote("run",task).addBoth(return_worker_to_pool).chainDeferred(deferred)
    #@-node:gcross.20091013163748.4169:distribute_tasks
    #@-others
#@-node:gcross.20091013163748.4166:Controller
#@+node:gcross.20091013163748.4174:Worker
class Worker(Referenceable):
    #@    @+others
    #@+node:gcross.20091013163748.4177:__slots__
    __slots__ = []
    #@-node:gcross.20091013163748.4177:__slots__
    #@+node:gcross.20091013163748.4175:remote_run
    def remote_run(self,task):
        return task()
    #@-node:gcross.20091013163748.4175:remote_run
    #@-others
#@-node:gcross.20091013163748.4174:Worker
#@-node:gcross.20091013163748.4165:Classes
#@-others

__all__ = ["startController","startWorker"]
#@-node:gcross.20091013163748.4164:@thin grid.py
#@-leo
