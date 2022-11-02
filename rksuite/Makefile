F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=rksuite.f

OBJ=$(SRC:%.f=%.o)

rksuite: $(SRC)
ifeq (,$(wildcard rksuite.f))
	wget https://netlib.sandia.gov/ode/rksuite/rksuite.f
endif
	patch -Nur /dev/null rksuite.f rksuite.f.patch || true
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	$(F77) -shared -o librksuite.so $(OBJ)
	install librksuite.so ../
clean:
	rm -f *.o *.so *~
