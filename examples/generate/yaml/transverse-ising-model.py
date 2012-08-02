import sys

if len(sys.argv) != 3:
    print "USAGE:",sys.argv[0]," <coupling strength> <number of sites>"
    print
    print "where the coupling strength is the relative weight of the neighbor-interaction"
    print "terms compared to the external field terms (which have weight 1)"
    sys.exit(1)

coupling_strength = sys.argv[1]
try:
    float(sys.argv[1])
except:
    print sys.argv[1],"is not a real number"
    sys.exit(1)
number_of_sites = int(sys.argv[2])

if number_of_sites == 1:
    print """--- # External field Hamiltonian for 1 site
sites:
  - physical dimension: 2
    left dimension:     1
    right dimension:    1
    matrices:
       - from: 1
         to:   1
         data: [1,0,0,-1]
"""
elif number_of_sites > 1:
    print """--- # Transverse-Ising model with coupling strength %s for %i sites
paulis:
  - &I [1,0,0,1]
  - &Z [1,0,0,-1]
  - &X1 [0,1,1,0]
  - &X2 [0,%s,%s,0]
sites:
  - physical dimension: 2
    left dimension:     1
    right dimension:    3
    matrices:
       - from: 1
         to:   1
         data: *I
       - from: 1
         to:   2
         data: *X1
       - from: 1
         to:   3
         data: *Z
  - physical dimension: 2
    left dimension:     3
    right dimension:    3
    matrices:
       - from: 1
         to:   1
         data: *I
       - from: 1
         to:   3
         data: *Z
       - from: 1
         to:   2
         data: *X1
       - from: 2
         to:   3
         data: *X2
       - from: 3
         to:   3
         data: *I
  - physical dimension: 2
    left dimension:     3
    right dimension:    1
    matrices:
       - from: 1
         to:   1
         data: *Z
       - from: 2
         to:   1
         data: *X2
       - from: 3
         to:   1
         data: *I
sequence: %s
""" % (coupling_strength,number_of_sites,coupling_strength,coupling_strength,[1] + [2]*(number_of_sites-2) + [3])
