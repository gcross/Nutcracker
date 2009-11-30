#@+leo-ver=4-thin
#@+node:gcross.20091111171052.1607:@thin Makefile
#@@language Makefile
#@@tabwidth 4

default: test benchmark

%: %.hs Makefile
	ghc --make $< -O2 -lvmps -lgfortran -lblas -llapack -larpack

clean:
	rm -f test
#@-node:gcross.20091111171052.1607:@thin Makefile
#@-leo
