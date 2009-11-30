#@+leo-ver=4-thin
#@+node:gcross.20091110205054.1950:@thin Makefile
#@@language Makefile
#@@tabwidth 4

test: vmps.so

vmps.so: ../../src/core.f95 Makefile
	f2py -m vmps $< -c -lgfortran -lblas -llapack -larpack

clean:
	rm -f vmps.so
#@-node:gcross.20091110205054.1950:@thin Makefile
#@-leo
