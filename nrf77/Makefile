F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=zbrent.f

OBJ=$(SRC:%.f=%.o)

nrf77:
ifeq (,$(wildcard $(SRC)))
	@echo 'Before proceeding, copy the source into this directory:' $(SRC) && exit 1
endif
	patch -Nur /dev/null zbrent.f zbrent.f.patch || true
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	$(F77) -shared -o libnrf77.so $(OBJ)
	install libnrf77.so ../

clean:
	rm -f *.o *.so *~