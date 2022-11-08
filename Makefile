F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

CC=gcc
CFLAGS=-g

SRC=grazing_9.f akyuz_lib.f angularmom_lib.f barrierpos_s.f capturenew_s.f \
    condens_s.f convert_s.f date_axp_s.f driftnztdis_s.f edistapp_s.f \
    edista_s.f emass_s.f enzcor_s.f evap_s.f fys_lib.f interpolate_lib.f \
    leg1new_s.f leg2new_s.f leg3new_s.f leg41new_s.f leg42new_s.f leg43new_s.f \
    leg44new_s.f leg45new_s.f leg4_s.f math_lib.f mavnzdis_s.f mavtdist_s.f \
    nzdistri_s.f nzedisa_s.f nztdis_s.f population_s.f probbarnew_s.f \
    qvaletc_s.f scatangleavnew_s.f selectnz_s.f tdistri_s.f time_axp_s.f \
    transmnew_s.f treslim_s.f wilsh_nzplotnew_s.f wilsh_plotnew_s.f wilsh_s.f

OBJ=$(SRC:%.f=%.o)


grazing: grazcode $(SRC) grazing.o libnag.so nrf77.so rksuite.so quadpack.so amos.so pchip.so
	$(F77) $(FFLAGS) -o grazing_9r $(OBJ) -L. -lnag -lnrf77 -lrksuite -lquadpack -lamos -lpchip -lm

grazcode:
ifeq (,$(wildcard ../grazing_jun2005.tar.gz))
	wget http://personalpages.to.infn.it/~nanni/grazing/grazing_jun2005.tar.gz -P ../
	tar -zxvf ../grazing_jun2005.tar.gz -C ../
	mv -f makefile makefile.orig
endif

grazing.o: $(SRC)
	patch -Nur /dev/null fys_lib.f fys_lib.f.patch || true
	patch -Nur /dev/null emass_s.f emass_s.f.patch || true
	$(F77) $(FFLAGS) -c $(SRC)

libnag.so: fnag.f cnag.c
	$(F77) $(FFLAGS) -fPIC -c fnag.f
	$(CC) $(CFLAGS) -fPIC -c cnag.c
	$(F77) $(FFLAGS) -fPIC -c fourpt.f
	$(F77) -shared -o libnag.so fnag.o cnag.o fourpt.o

rksuite.so:
	$(MAKE) -C rksuite

nrf77.so:
	$(MAKE) -C nrf77

quadpack.so:
	$(MAKE) -C quadpack

amos.so:
	$(MAKE) -C amos

pchip.so:
	$(MAKE) -C pchip

clean:
	rm -f *.o *.so fort.* *~ grazing_9r
	$(MAKE) -C nrf77 clean
	$(MAKE) -C rksuite clean
	$(MAKE) -C quadpack clean
	$(MAKE) -C amos clean
	$(MAKE) -C pchip clean
