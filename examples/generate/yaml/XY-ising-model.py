from __future__ import print_function

import sys

if len(sys.argv) != 3:
    print("USAGE:",sys.argv[0],"<Y coupling strength> <number of sites>")
    sys.exit(1)

spin_coupling = float(sys.argv[1])
number_of_sites = int(sys.argv[2])
if number_of_sites < 2:
    print("The number of sites must be at least 2.")
    sys.exit(1)

print("""--- # XY-model for %i sites
paulis:
  - &I [1,0,0,1]
  - &X1 [0,1,1,0]
  - &X2 [0,-1,-1,0]
  - &Z1 [1,0,0,-1]
  - &Z2 [%f,0,0,%f]
sites:
  - physical dimension: 2
    left dimension:     1
    right dimension:    4
    matrices:
       - from: 1
         to:   1
         data: *I
       - from: 1
         to:   2
         data: *X1
       - from: 1
         to:   3
         data: *Z1
  - physical dimension: 2
    left dimension:     4
    right dimension:    4
    matrices:
       - from: 1
         to:   1
         data: *I
       - from: 1
         to:   2
         data: *X1
       - from: 1
         to:   3
         data: *Z1
       - from: 2
         to:   4
         data: *X2
       - from: 3
         to:   4
         data: *Z2
       - from: 4
         to:   4
         data: *I
  - physical dimension: 2
    left dimension:     4
    right dimension:    1
    matrices:
       - from: 2
         to:   1
         data: *X2
       - from: 3
         to:   1
         data: *Z2
       - from: 4
         to:   1
         data: *I
sequence: %s
""" % (number_of_sites,-spin_coupling,spin_coupling,[1] + [2]*(number_of_sites-2) + [3]))
