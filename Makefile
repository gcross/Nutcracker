#@+leo-ver=4-thin
#@+node:gcross.20091106154604.9811:@thin Makefile
#@@language Makefile
#@@tabwidth 4

default: objects

include paths.mk
include options.mk

obj/%.o: src/%.c Makefile
	${CC} ${CFLAGS} -c $< -o $@
obj/%.o: src/%.hs Makefile
	${HC} -iobj ${HFLAGS} -c $< -o $@ -ohi obj/$*.hi
obj/%.o: src/%.f95 Makefile                 
	${FC} ${FFLAGS} -c $< -o $@
obj/%.o: src/%.f90 Makefile                 
	${FC} ${FFLAGS} -c $< -o $@
obj/%.o: src/%.f Makefile                   
	${FC} ${FFLAGS} -c $< -o $@

OBJS = \
	obj/VMPS/Miscellaneous.o \
	obj/VMPS/Tensors.o \
	obj/VMPS/Wrappers.o \
	obj/VMPS/EnergyMinimizationChain.o \
	obj/core.o \
	obj/core-wrapper.o \

LIBS = -lblas -llapack -larpack -lgfortran

objects: ${OBJS}

clean:
	rm -f mods/* obj/* obj/VMPS/* lib/* src/*.mod
#@-node:gcross.20091106154604.9811:@thin Makefile
#@-leo
