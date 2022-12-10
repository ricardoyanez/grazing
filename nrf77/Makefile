F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=zbrent.f bessi0.f bessi1.f

OBJ=$(SRC:%.f=%.o)

nrf77:
ifeq (,$(wildcard $(SRC)))
	@echo 'Before proceeding, copy the source into this directory:' $(SRC) && exit 1
endif
	patch -Nur /dev/null zbrent.f zbrent.f.patch || true
	patch -Nur /dev/null bessi0.f bessi0.f.patch || true
	patch -Nur /dev/null bessi1.f bessi1.f.patch || true
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	$(F77) -shared -o libnrf77.so $(OBJ)
	install libnrf77.so ../

clean:
	cp -af zbrent.f.orig zbrent.f
	cp -af bessi0.f.orig bessi0.f
	cp -af bessi1.f.orig bessi1.f
	rm -f *.o *.so *~
