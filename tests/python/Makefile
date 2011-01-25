#@+leo-ver=5-thin
#@+node:gcross.20091110205054.1950: * @thin Makefile
#@@language Makefile
#@@tabwidth 4

test: vmps.so

vmps.so: ../../sources/common/core.f95 Makefile
	f2py -m vmps $< -c -lgfortran -lblas -llapack -larpack --f90flags="-fbounds-check -fimplicit-none"

clean:
	rm -f vmps.so
#@-leo
