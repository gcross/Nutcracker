#@+leo-ver=4-thin
#@+node:gcross.20091013184459.1473:@thin worker.py
#@@language Python

import warnings
warnings.simplefilter('ignore', DeprecationWarning)

#@<< Import needed modules >>
#@+node:gcross.20091013184459.1477:<< Import needed modules >>
from foolscap.api import Tub, Referenceable, Copyable, RemoteCopy, DeadReferenceError
from twisted.internet import reactor
from twisted.internet.defer import Deferred, inlineCallbacks, returnValue

from trial import *
from models import *
#@-node:gcross.20091013184459.1477:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091013184459.1474:Functions
#@+node:gcross.20091013163748.4173:becomeWorker
def becomeWorker(furl):
    tub = Tub()
    tub.startService()
    @inlineCallbacks
    def main():
        controller = yield tub.getReference(furl)
        yield controller.callRemote("add_worker",Worker())
        @inlineCallbacks
        def ping():
            try:
                yield controller.callRemote("ping")
            except DeadReferenceError:
                reactor.stop()
            reactor.callLater(1,ping)
        yield ping()
    reactor.callLater(0,main)
    reactor.run()
#@-node:gcross.20091013163748.4173:becomeWorker
#@-node:gcross.20091013184459.1474:Functions
#@+node:gcross.20091013184459.1475:Classes
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
#@-node:gcross.20091013184459.1475:Classes
#@-others

__all__ = ["becomeWorker"]

if __name__=="__main__":
    from sys import argv
    becomeWorker(argv[1])
#@-node:gcross.20091013184459.1473:@thin worker.py
#@-leo
