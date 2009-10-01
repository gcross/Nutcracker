#@+leo-ver=4-thin
#@+node:gcross.20090930213318.1312:@thin compute-TI-spectrum.py
#@@language Python

from numpy.linalg import *
from numpy import *
from scipy.sparse.linalg.eigen.arpack import eigen, eigen_symmetric


import time, sys

if len(sys.argv) < 3:
    sys.exit("Usage: %s  number_of_sites  lambda  [number_of_levels_to_display]" % sys.argv[0])

number_of_sites = int(sys.argv[1])
lam = float(sys.argv[2])
if len(sys.argv) >= 4:
    levels_to_display = int(sys.argv[3])
else:
    levels_to_display = 1

I = array([1,0,0,1])
X = array([0,1,1,0])
Z = array([1,0,0,-1])

terms = []

print "Building list of terms..."

for i in range(number_of_sites-1):
    pass
    terms.append( [I]*i + [-X] + [I]*(number_of_sites-i-1) )
    terms.append( [I]*i + [-lam*Z,Z] + [I]*(number_of_sites-i-2) )

terms.append([I]*(number_of_sites-1)+[-X])
#terms.append([-lam*Z] + [I]*(number_of_sites-2)+[Z])

print "Building NON-PERIODIC (OBC) Hamiltonian..."

t = time.time()
H = reduce(lambda x,y: x+y, map(lambda term: reduce(multiply.outer,term), terms))
print time.time()-t,"seconds taken."

matrix_dimension = 2**number_of_sites

print "Reshaping..."

H.shape = (2,)*(number_of_sites*2) 

every_other_index = 2*arange(number_of_sites)

new_indices = every_other_index.tolist() + (every_other_index+1).tolist()

#print new_indices
#print H.shape
#print prod(H.shape)
#print H.strides

H.strides = array(H.strides)[ range(0,len(H.strides),2)+range(1,len(H.strides)+1,2) ]

#H = H.transpose( new_indices )

#print H.strides

H = H.reshape( (matrix_dimension,matrix_dimension) )

t = time.time()

print "Finding eigenvalues..."

#evals = eigvalsh(H)
#evals.sort()

evals = eigen_symmetric(H,k=levels_to_display,return_eigenvectors=False,which='SA').tolist()
evals.reverse()

print time.time()-t,"seconds taken."
print
print "Energy levels are:"
print "------------------"

print
for eval in evals[:levels_to_display]:
    print "\t",eval
print

print time.time()-t,"seconds taken."
#@-node:gcross.20090930213318.1312:@thin compute-TI-spectrum.py
#@-leo
