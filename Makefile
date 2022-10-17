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
	$(F77) $(FFLAGS) $(OBJ) libnag.so -o grazing

grazing.o: $(SRC)
	$(F77) $(FFLAGS) -c $(SRC)

NAG_OBJ=x05baf.o

NAG_SRC=$(NAG_OBJ:%.o=%.c)

libnag.so: nag.f d02bbf.f90 $(NAG_SRC) rksuite.o
	$(F77) $(FFLAGS) -fPIC -c nag.f
	$(F90) $(FFLAGS) -fPIC -c d02bbf.f90
	$(CC) $(CFLAGS) -fPIC -c $(NAG_SRC)
	$(F77) -shared -o libnag.so nag.o d02bbf.o rksuite.o $(NAG_OBJ)

rksuite.o:
ifeq (,$(wildcard ./rksuite.pdf))
	wget https://netlib.sandia.gov/ode/rksuite/rksuite.doc -O rksuite.doc
endif
ifeq (,$(wildcard ./rksuite.f))
	wget https://netlib.sandia.gov/ode/rksuite/rksuite.f -O rksuite.f
endif
	$(F77) $(FFLAGS) -fPIC -c rksuite.f
	/usr/bin/lowriter --headless --convert-to pdf:writer_pdf_Export rksuite.doc
	rm -f rksuite.doc

patch:
	diff -Naur fys_lib.f.orig fys_lib.f > fys_lib.f.patch

clean:
	rm -f *.o *.so rksuite.f rksuite.pdf
