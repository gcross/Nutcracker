#@+leo-ver=4-thin
#@+node:gcross.20091014143858.1490:@thin database.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091014143858.1508:<< Import needed modules >>
from ConfigParser import SafeConfigParser
from itertools import izip
from uuid import uuid4
#from pg import connect, unescape_bytea, escape_bytea
from psycopg2 import connect, Binary
from numpy import frombuffer, complex128
#@-node:gcross.20091014143858.1508:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091014143858.1491:Classes
#@+node:gcross.20091014143858.1492:VMPSDatabase
class VMPSDatabase(object):
    #@    @+others
    #@+node:gcross.20091014143858.1493:__slots__
    __slots__ = [
        "connection",
        "cursor",
    ]
    #@-node:gcross.20091014143858.1493:__slots__
    #@+node:gcross.20091014143858.1494:__init__
    def __init__(self,header="DEFAULT",*args,**keywords):
        config = SafeConfigParser()
        config.read("database.cfg")
        keywords.update(dict(config.items(header)))
        self.connection = connect(*args,**keywords)
        self.cursor = self.connection.cursor()
    #@-node:gcross.20091014143858.1494:__init__
    #@+node:gcross.20091014143858.1507:save_state/load_state
    def save_state(self,state_site_tensors):
        state_id = str(uuid4())
        self.cursor.execute(
            "insert into state_site_tensors (state_id,site_number,physical_dimension,left_bandwidth_dimension,right_bandwidth_dimension,data) values "
            +
            ",".join(["('%s',%i,%i,%i,%i,E%s)" % ((state_id,site_number,) + tensor.shape + (Binary(tensor.tostring()),)) for (site_number,tensor) in enumerate(state_site_tensors)])
        )
        assert(self.cursor.rowcount == len(state_site_tensors))
        return state_id

    def load_state(self,state_id):
        self.cursor.execute("SELECT data,physical_dimension,left_bandwidth_dimension,right_bandwidth_dimension FROM state_site_tensors WHERE state_id='%s' ORDER BY site_number" % state_id)
        return [
            frombuffer(result[0],dtype=complex128).reshape(result[1:])
            for result in self.cursor.fetchall()
        ]
    #@-node:gcross.20091014143858.1507:save_state/load_state
    #@+node:gcross.20091014143858.1509:save_solution/load_solution
    def save_solution(self,energy_levels,state_site_tensors_list):
        solution_id = str(uuid4())
        self.cursor.execute(
            "insert into solutions (solution_id,level_number,state_id,energy) values "
            +
            ",".join([
                "('%s',%i,'%s',%f)" % (solution_id,level_number,self.save_state(state_site_tensors),energy)
                for (level_number,(energy,state_site_tensors)) in enumerate(izip(energy_levels,state_site_tensors_list))
            ])
        )
        assert(self.cursor.rowcount == len(state_site_tensors_list))
        return solution_id

    def load_solution(self,solution_id):
        self.cursor.execute("select energy, state_id from solutions where solution_id='{0}' order by level_number asc".format(solution_id))
        return [(energy,self.load_state(state_id)) for (energy,state_id) in self.cursor.fetchall()]
    #@-node:gcross.20091014143858.1509:save_solution/load_solution
    #@-others
#@-node:gcross.20091014143858.1492:VMPSDatabase
#@-node:gcross.20091014143858.1491:Classes
#@-others

__all__ = ["VMPSDatabase"]
#@-node:gcross.20091014143858.1490:@thin database.py
#@-leo
