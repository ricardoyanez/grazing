F77=gfortran
FFLAGS=-g -std=legacy

SRC=dqagi.f dqagie.f dqelg.f dqk15i.f dqpsrt.f d1mach.f i1mach.f xerror.f \
    xerrwv.f xerprt.f xersav.f xerabt.f xerctl.f xgetua.f j4save.f s88fmt.f \
    fdump.f

OBJ=$(SRC:%.f=%.o)

quadpack:
ifeq (,$(wildcard ./dqagi.f))
	wget https://netlib.org/quadpack/dqagi.f -O ./dqagi.f
	wget https://netlib.org/quadpack/dqagie.f -O ./dqagie.f
	wget https://netlib.org/quadpack/dqelg.f -O ./dqelg.f
	wget https://netlib.org/quadpack/dqk15i.f -O ./dqk15i.f
	wget https://netlib.org/quadpack/dqpsrt.f -O ./dqpsrt.f
	wget https://netlib.org/port/d1mach.f -O ./d1mach.f
	wget https://netlib.org/port/i1mach.f -O ./i1mach.f
	wget https://netlib.org/alliant/quad/xerror.f -O ./xerror.f
	wget https://netlib.org/alliant/quad/xerrwv.f -O ./xerrwv.f
	wget https://netlib.org/alliant/quad/xerprt.f -O ./xerprt.f
	wget https://netlib.org/alliant/quad/xersav.f -O ./xersav.f
	wget https://netlib.org/alliant/quad/xerabt.f -O ./xerabt.f
	wget https://netlib.org/alliant/quad/xerctl.f -O ./xerctl.f
	wget https://netlib.org/alliant/quad/xgetua.f -O ./xgetua.f
	wget https://netlib.org/alliant/quad/j4save.f -O ./j4save.f
	wget https://netlib.org/alliant/quad/s88fmt.f -O ./s88fmt.f
	wget https://netlib.org/alliant/quad/fdump.f -O ./fdump.f
endif
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	$(F77) -shared -o libquadpack.so $(OBJ)
	install libquadpack.so ../

clean:
	rm -f *.o *.so
