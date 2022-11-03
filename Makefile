F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

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

grazing: $(SRC) grazing.o libnag.so nrf77.so rksuite.so quadpack.so amos.so pchim.so
	$(F77) $(FFLAGS) -o grazing_9r $(OBJ) -L. -lnag -lnrf77 -lrksuite -lquadpack -lamos -lpchim -lm

grazing.o: $(SRC)
	patch -Nur /dev/null fys_lib.f fys_lib.f.patch || true
	patch -Nur /dev/null emass_s.f emass_s.f.patch || true
	$(F77) $(FFLAGS) -c $(SRC)

libnag.so: fnag.f cnag.c
	$(F77) $(FFLAGS) -fPIC -c fnag.f
	$(CC) $(CFLAGS) -fPIC -c cnag.c
	$(F77) -shared -o libnag.so fnag.o cnag.o

rksuite.so:
	$(MAKE) -C rksuite

nrf77.so:
	$(MAKE) -C nrf77

quadpack.so:
	$(MAKE) -C quadpack

amos.so:
	$(MAKE) -C amos

pchim.so:
	$(MAKE) -C pchim

clean:
	rm -f *.o *.so fort.* *~ grazing_9r
	$(MAKE) -C nrf77 clean
	$(MAKE) -C rksuite clean
	$(MAKE) -C quadpack clean
