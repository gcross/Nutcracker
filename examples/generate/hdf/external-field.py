from __future__ import print_function

import h5py, sys
from numpy import array, complex128

if len(sys.argv) != 4:
    print("USAGE:",sys.argv[0]," <coupling strength> <number of sites> <output file>")
    sys.exit(1)


try:
    coupling_strength = float(sys.argv[1])
except:
    print(sys.argv[1],"is not a real number")
    sys.exit(1)
number_of_sites = int(sys.argv[2])
output_file = sys.argv[3]

I = [1,0,0,1]
Z = [1,0,0,-1]

with h5py.File(output_file,"w-") as f: # Fail if exists;  use "w" to truncate if exists

    if number_of_sites == 1:
        f["sequence"] = [0]

        sites = f.create_group("sites")
        sites.attrs["size"] = 1

        site = sites.create_group("0")
        site.attrs["left dimension"] = 1
        site.attrs["right dimension"] = 1
        site["matrices"] = array([I],dtype=complex128).reshape(1,2,2)
        site["indices"] = array([1,1]).reshape(1,2)
    else:
        f["sequence"] = [0] + [1]*(number_of_sites-2) + [2]

        sites = f.create_group("sites")
        sites.attrs["size"] = 3

        site = sites.create_group("0")
        site.attrs["left dimension"] = 1
        site.attrs["right dimension"] = 2
        site["matrices"] = array([I,    Z  ],dtype=complex128).reshape(2,2,2)
        site["indices"] = array(  [1,1,  1,2]).reshape(2,2)

        site = sites.create_group("1")
        site.attrs["left dimension"] = 2
        site.attrs["right dimension"] = 2
        site["matrices"] = array([I,    Z,    I  ],dtype=complex128).reshape(3,2,2)
        site["indices"] = array(  [1,1,  1,2,  2,2]).reshape(3,2)

        site = sites.create_group("2")
        site.attrs["left dimension"] = 2
        site.attrs["right dimension"] = 1
        site["matrices"] = array([I,    Z  ],dtype=complex128).reshape(2,2,2)
        site["indices"] = array(  [2,1,  1,1]).reshape(2,2)
