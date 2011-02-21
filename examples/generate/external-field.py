#@+leo-ver=5-thin
#@+node:gcross.20110220182654.1999: * @thin external-field.py
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
sites:
  - physical dimension: 2
    left dimension:     1
    right dimension:    2
    matrices:
       - from: 1
         to:   1
         data: [1,0,0,1]
       - from: 1
         to:   2
         data: [1,0,0,-1]
  - physical dimension: 2
    left dimension:     2
    right dimension:    2
    matrices:
       - from: 1
         to:   1
         data: [1,0,0,1]
       - from: 1
         to:   2
         data: [1,0,0,-1]
       - from: 2
         to:   2
         data: [1,0,0,1]
  - physical dimension: 2
    left dimension:     2
    right dimension:    1
    matrices:
       - from: 1
         to:   1
         data: [1,0,0,-1]
       - from: 2
         to:   1
         data: [1,0,0,1]
sequence: %s
""" % (number_of_sites,[1] + [2]*(number_of_sites-2) + [3])
#@-leo
