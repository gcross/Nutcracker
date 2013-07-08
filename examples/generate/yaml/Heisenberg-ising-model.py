from __future__ import print_function

import sys

if len(sys.argv) != 4:
    print("USAGE:",sys.argv[0],"<Y coupling strength> <Z coupling strength> <number of sites>")
    sys.exit(1)

Y_spin_coupling = float(sys.argv[1])
Z_spin_coupling = float(sys.argv[2])
number_of_sites = int(sys.argv[3])
if number_of_sites < 2:
    print("The number of sites must be at least 2.")
    sys.exit(1)

print("""--- # Heisenberg-model for %i sites
paulis:
  - &I [1,0,0,1]
  - &X1 [0,1,1,0]
  - &X2 [0,-1,-1,0]
  - &Y1 [0,[0,-1],[0,1],0]
  - &Y2 [0,[0,%f],[0,%f],0]
  - &Z1 [1,0,0,-1]
  - &Z2 [%f,0,0,%f]
sites:
  - physical dimension: 2
    left dimension:     1
    right dimension:    5
    matrices:
       - from: 1
         to:   1
         data: *I
       - from: 1
         to:   2
         data: *X1
       - from: 1
         to:   3
         data: *Y1
       - from: 1
         to:   4
         data: *Z1
  - physical dimension: 2
    left dimension:     5
    right dimension:    5
    matrices:
       - from: 1
         to:   1
         data: *I
       - from: 1
         to:   2
         data: *X1
       - from: 1
         to:   3
         data: *Y1
       - from: 1
         to:   4
         data: *Z1
       - from: 2
         to:   5
         data: *X2
       - from: 3
         to:   5
         data: *Y2
       - from: 4
         to:   5
         data: *Z2
       - from: 5
         to:   5
         data: *I
  - physical dimension: 2
    left dimension:     5
    right dimension:    1
    matrices:
       - from: 2
         to:   1
         data: *X2
       - from: 3
         to:   1
         data: *Y2
       - from: 4
         to:   1
         data: *Z2
       - from: 5
         to:   1
         data: *I
sequence: %s
""" % (number_of_sites,Y_spin_coupling,-Y_spin_coupling,-Z_spin_coupling,Z_spin_coupling,[1] + [2]*(number_of_sites-2) + [3]))
