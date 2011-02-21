#@+leo-ver=5-thin
#@+node:gcross.20110220182654.2002: * @thin external-field-using-anchors.py
#@@language python
import sys

if len(sys.argv) != 2:
    print "USAGE:",sys.argv[0]," <number of sites>"
    sys.exit(1)

number_of_sites = int(sys.argv[1])

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
    print """--- # External field Hamiltonian for %i sites
paulis:
  - &I [1,0,0,1]
  - &Z [1,0,0,-1]
sites:
  - physical dimension: 2
    left dimension:     1
    right dimension:    2
    matrices:
       - from: 1
         to:   1
         data: *I
       - from: 1
         to:   2
         data: *Z
  - physical dimension: 2
    left dimension:     2
    right dimension:    2
    matrices:
       - from: 1
         to:   1
         data: *I
       - from: 1
         to:   2
         data: *Z
       - from: 2
         to:   2
         data: *I
  - physical dimension: 2
    left dimension:     2
    right dimension:    1
    matrices:
       - from: 1
         to:   1
         data: *Z
       - from: 2
         to:   1
         data: *I
sequence: %s
""" % (number_of_sites,[1] + [2]*(number_of_sites-2) + [3])
#@-leo
