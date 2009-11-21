#@+leo-ver=4-thin
#@+node:gcross.20091106154604.9811:@thin Makefile
#@@language Makefile
#@@tabwidth 4

default: lib/libvmps.a

include paths.mk
include options.mk

obj/%.o: src/%.c Makefile
	${CC} ${CFLAGS} -c $< -o $@
obj/%.o: src/%.hs Makefile
	${HC} -ihaskint ${HFLAGS} -c $< -o $@ -ohi haskint/$*.hi
obj/%.o: src/%.f95 Makefile                 
	${FC} ${FFLAGS} -c $< -o $@
obj/%.o: src/%.f90 Makefile                 
	${FC} ${FFLAGS} -c $< -o $@
obj/%.o: src/%.f Makefile                   
	${FC} ${FFLAGS} -c $< -o $@

OBJS = \
	obj/VMPS/Miscellaneous.o \
	obj/VMPS/Pauli.o \
	obj/VMPS/Tensors/Implementation.o \
	obj/VMPS/Tensors.o \
	obj/VMPS/OperatorConstruction.o \
	obj/VMPS/Wrappers.o \
	obj/VMPS/EnergyMinimizationChain.o \
	obj/VMPS/Algorithms.o \
	obj/core.o \
	obj/core-wrapper.o \

LIBS = -lblas -llapack -larpack -lgfortran

lib/libvmps.a: ${OBJS}
	ar r lib/libvmps.a ${OBJS}

clean:
	rm -f mods/*.mod obj/*.o obj/VMPS/*.o obj/VMPS/Tensors/*.o haskint/*.o haskint/VMPS/*.hi haskint/VMPS/Tensors/*.hi lib/*.a src/*.mod
#@-node:gcross.20091106154604.9811:@thin Makefile
#@-leo
