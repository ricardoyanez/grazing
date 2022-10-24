F77=gfortran
F90=gfortran
#FFLAGS=-finit-local-zero -fno-automatic -fno-backslash -fdollar-ok
FFLAGS=-g

CC=gcc
CFLAGS=-g

OBJ=grazing_9.o \
    akyuz_lib.o \
    angularmom_lib.o \
    barrierpos_s.o \
    capturenew_s.o \
    condens_s.o \
    convert_s.o \
    date_axp_s.o \
    driftnztdis_s.o \
    edistapp_s.o \
    edista_s.o \
    emass_s.o \
    enzcor_s.o \
    evap_s.o \
    fys_lib.o \
    interpolate_lib.o \
    leg1new_s.o \
    leg2new_s.o \
    leg3new_s.o \
    leg41new_s.o \
    leg42new_s.o \
    leg43new_s.o \
    leg44new_s.o \
    leg45new_s.o \
    leg4_s.o \
    math_lib.o \
    mavnzdis_s.o \
    mavtdist_s.o \
    nzdistri_s.o \
    nzedisa_s.o \
    nztdis_s.o \
    population_s.o \
    probbarnew_s.o \
    qvaletc_s.o \
    scatangleavnew_s.o \
    selectnz_s.o \
    tdistri_s.o \
    time_axp_s.o \
    transmnew_s.o \
    treslim_s.o \
    wilsh_nzplotnew_s.o \
    wilsh_plotnew_s.o \
    wilsh_s.o

SRC=$(OBJ:%.o=%.f)

grazing: $(SRC) grazing.o libnag.so
	$(F77) $(FFLAGS) $(OBJ) libnag.so -o grazing_9r -lnrf77 -lm

grazing.o: $(SRC)
	$(F77) $(FFLAGS) -c $(SRC)

libnag.so: fnag.f cnag.c rksuite.o nrf77.so quadpack.so
	$(F77) $(FFLAGS) -fPIC -c fnag.f
	$(CC) $(CFLAGS) -fPIC -c cnag.c
	$(F77) -shared -o libnag.so libnrf77.so libquadpack.so fnag.o cnag.o rksuite.o

rksuite.o: rksuite.f
ifeq (,$(wildcard ./rksuite.f))
	wget https://netlib.sandia.gov/ode/rksuite/rksuite.f -O rksuite.f
endif
	$(F77) $(FFLAGS) -fPIC -c rksuite.f

nrf77.so: zbrent.for
	$(F77) $(FFLAGS) -fPIC -c zbrent.for
	$(F77) -shared -o libnrf77.so zbrent.o

quadpack.so:
	$(MAKE) -C quadpack

clean:
	rm -f *.o *.so fort.* *~ grazing_9r
	$(MAKE) -C quadpack clean
