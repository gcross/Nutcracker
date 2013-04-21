from __future__ import print_function

import sys

if len(sys.argv) != 3:
    print("USAGE:",sys.argv[0]," <number of approximation terms> <number of sites>")
    print()
    print(
"""\
where the number of approximation terms sets the number of terms used to
approximate the 1/r^2 dipole coupling field (single digits should suffice)
and the number of sites sets the number of sites in the system."""
    )
    sys.exit(1)

try:
    n = int(sys.argv[1])
    assert n >= 1
except:
    print(sys.argv[1],"is not a positive integer")
    sys.exit(1)

try:
    number_of_sites = int(sys.argv[2])
    assert number_of_sites >= 2
except:
    print(sys.argv[2],"is not a positive integer that is at least 2")
    sys.exit(1)

from numpy import arange, ones, sum, zeros, array
from scipy.optimize import leastsq

rvec = arange(1,number_of_sites+1)

def f(x):
    a = x[:n].reshape(1,n)
    b = x[n:].reshape(1,n)
    return sum(a*b**(rvec.reshape(number_of_sites,1)-1),axis=1) - 1/rvec**2

def Df(x):
    a = x[:n].reshape(1,n)
    b = x[n:].reshape(1,n)
    Df = zeros((number_of_sites,2*n),dtype=float)
    Df[:,:n] = b**(rvec.reshape(number_of_sites,1)-1)
    Df[:,n:] = a*(rvec.reshape(number_of_sites,1)-1)*b**(rvec.reshape(number_of_sites,1)-2)
    return Df

xf = leastsq(f,ones(2*n,dtype=float),Dfun=Df)[0]
print("# The sum of the errors squared is",sum(f(xf)**2))
print()
a = xf[:n]
b = xf[n:]

def print_start_terms():
    print("""\
       - from: 1
         to:   1
         data: *I""")
    for i in range(n):
        print("""\
       - from: 1
         to:   {}
         data: *X
       - from: 1
         to:   {}
         data: *Y
       - from: 1
         to:   {}
         data: *Z""".format(
            1+1+i*3,
            1+2+i*3,
            1+3+i*3,
        ))

def print_end_terms(last_site):
    if last_site:
        end_index = 1
    else:
        end_index = 2+3*n
    print("""\
       - from: {}
         to:   {}
         data: *I""".format(3*n+2,end_index))
    for i in range(n):
        print("""\
       - from: {0:}
         to:   {3:}
         data: [0,{4:},{4:},0]
       - from: {1:}
         to:   {3:}
         data: [0,[0,{5:}],[0,{4:}],0]
       - from: {2:}
         to:   {3:}
         data: [{4:},0,0,{5:}]""".format(
            1+1+i*3,
            1+2+i*3,
            1+3+i*3,
            end_index,
            a[i],
            -a[i],
        ))

def print_middle_terms():
    for i in range(n):
        print("""\
       - from: {0:}
         to:   {0:}
         data: [{3:},0,0,{3:}]
       - from: {1:}
         to:   {1:}
         data: [{3:},0,0,{3:}]
       - from: {2:}
         to:   {2:}
         data: [{3:},0,0,{3:}]""".format(
            1+1+i*3,
            1+2+i*3,
            1+3+i*3,
            b[i],
        ))

print("""\
--- # Long range dipole coupling field for 1 site
paulis:
  - &I [1,0,0,1]
  - &X [0,1,1,0]
  - &Y [0,[0,-1],[0,1],0]
  - &Z [1,0,0,-1]
sites:
# leftmost site
  - physical dimension: 2
    left dimension:     1
    right dimension:    {}
    matrices:""".format(3*n+2))
print_start_terms()

print("""\
# rightmost site
  - physical dimension: 2
    left dimension:     {}
    right dimension:    1
    matrices:""".format(3*n+2))
print_end_terms(True)

print("""\
# middle site
  - physical dimension: 2
    left dimension:     {0:}
    right dimension:    {0:}
    matrices:""".format(3*n+2))
print_start_terms()
print_middle_terms()
print_end_terms(False)

print("""\
sequence: {}""".format([1] + (number_of_sites-2)*[3] + [2]))
