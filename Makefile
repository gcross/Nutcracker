#@+leo-ver=4-thin
#@+node:gcross.20091106154604.9811:@thin Makefile
#@@language Makefile
#@@tabwidth 4

all: lib/vmps.so

include paths.mk
include options.mk

SOURCES = \
	src/utils.f95 \
	src/contractors.f95 \
	src/optimizer.f95 \
	src/randomizer.f95 \
	src/normalizer.f95 \

OBJECTS = \
	obj/utils.o \
	obj/contractors.o \
	obj/optimizer.o \
	obj/randomizer.o \
	obj/normalizer.o \
	obj/wrappers/vmpsmodule.o \
	obj/wrappers/vmps-f2pywrappers2.o \
	obj/wrappers/fortranobject.o

src/wrappers/vmpsmodule.c src/wrapers/vmps-f2pywrappers2.f90: ${SOURCES} Makefile
	${F2PY} ${SOURCES} -m vmps --build-dir src/wrappers

obj/%.o: src/%.c Makefile
	${CC} ${FLAGS} ${CFLAGS} -c $< -o $@
obj/%.o: src/%.f95 Makefile                 
	${FC} ${FLAGS} ${FFLAGS} -c $< -o $@
obj/%.o: src/%.f90 Makefile                 
	${FC} ${FLAGS} ${FFLAGS} -c $< -o $@
obj/%.o: src/%.f Makefile                   
	${FC} ${FLAGS} ${FFLAGS} -c $< -o $@

lib/vmps.so: ${OBJECTS} Makefile
	${FC} ${PYTHONLINK} ${FLAGS} -shared -o lib/vmps.so -lgfortran -lblas -llapack -larpack ${OBJECTS}

clean:
	rm -f obj/*/* lib/* src/*/*.mod src/wrappers/vpifmodule.c src/wrappers/vpif-f2pywrappers2.f90
#@-node:gcross.20091106154604.9811:@thin Makefile
#@-leo
