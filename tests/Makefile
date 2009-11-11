#@+leo-ver=4-thin
#@+node:gcross.20091110205054.1950:@thin Makefile
#@@language Makefile
#@@tabwidth 4

test: vmps.so

include ../paths.mk
include ../options.mk

vmps.so: ../src/core.f95 Makefile
	${F2PY} -m vmps $< -c -lgfortran -lblas -llapack -larpack

clean:
	rm -f vmps.so
#@-node:gcross.20091110205054.1950:@thin Makefile
#@-leo
