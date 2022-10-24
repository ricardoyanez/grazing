F77=gfortran
FFLAGS=-g

SRC=dqagie.f dqagi.f dqelg.f dqk15i.f dqpsrt.f

OBJ=$(SRC:%.f=%.o)

quadpack.so: $(SRC)
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	$(F77) -shared -o libquadpack.so $(OBJ)
	install libquadpack.so ../

clean:
	rm -f *.o *.so
