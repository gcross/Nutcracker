#@+leo-ver=4-thin
#@+node:gcross.20091106154604.9811:@thin Makefile
#@@language Makefile
#@@tabwidth 4

default: test

include paths.mk
include options.mk

obj/%.o: src/%.c Makefile
	${CC} ${CFLAGS} -c $< -o $@
obj/%.o: src/%.hs Makefile
	${HC} ${HFLAGS} -c $< -o $@
obj/%.o: src/%.f95 Makefile                 
	${FC} ${FFLAGS} -c $< -o $@
obj/%.o: src/%.f90 Makefile                 
	${FC} ${FFLAGS} -c $< -o $@
obj/%.o: src/%.f Makefile                   
	${FC} ${FFLAGS} -c $< -o $@

TESTOBJS = obj/test.o obj/core.o
LIBS = -lblas -llapack -larpack -lgfortran

test: ${TESTOBJS}
	${HC} ${TESTOBJS} ${LIBS} -o test

clean:
	rm -f mods/* obj/* lib/* src/*.mod
#@-node:gcross.20091106154604.9811:@thin Makefile
#@-leo
