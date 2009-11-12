#@+leo-ver=4-thin
#@+node:gcross.20091111171052.1607:@thin Makefile
#@@language Makefile
#@@tabwidth 4

default: test

include ../../paths.mk
include ../../options.mk

OBJS = ../../obj/VMPS.o ../../obj/core.o
LIBS = -lgfortran -lblas -llapack -larpack
PACKAGES = \
	-package HUnit \
	-package test-framework \
	-package QuickCheck \
	-package test-framework-hunit \
	-package test-framework-quickcheck2 \

test: test.hs ${OBJS} Makefile
	ghc  -i../../obj test.hs ${OBJS} ${LIBS} ${PACKAGES} -o test

clean:
	rm -f vmps.so
#@-node:gcross.20091111171052.1607:@thin Makefile
#@-leo
