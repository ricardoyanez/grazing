F77=gfortran
FFLAGS=-g

SRC=dqagi.f dqagie.f dqelg.f dqk15i.f dqpsrt.f

OBJ=$(SRC:%.f=%.o)

quadpack:
ifeq (,$(wildcard ./dqagi.f))
	wget https://netlib.org/quadpack/dqagi.f -O ./dqagi.f
	wget https://netlib.org/quadpack/dqagie.f -O ./dqagie.f
	wget https://netlib.org/quadpack/dqelg.f -O ./dqelg.f
	wget https://netlib.org/quadpack/dqk15i.f -O ./dqk15i.f
	wget https://netlib.org/quadpack/dqpsrt.f -O ./dqpsrt.f
endif
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	$(F77) -shared -o libquadpack.so $(OBJ)
	install libquadpack.so ../

clean:
	rm -f *.o *.so
