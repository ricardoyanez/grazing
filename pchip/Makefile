F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=dpchim.f dpchst.f dpchfe.f dchfev.f dpchfd.f dchfdv.f xermsg.f xerprn.f xerhlt.f xersve.f xercnt.f
OBJ=$(SRC:%.f=%.o)

quadpack:
ifeq (,$(wildcard dpchim.f))
	wget https://netlib.org/slatec/pchip/dpchim.f
endif
ifeq (,$(wildcard dpchst.f))
	wget https://netlib.org/slatec/pchip/dpchst.f
endif
ifeq (,$(wildcard dpchfe.f))
	wget https://netlib.org/slatec/pchip/dpchfe.f
endif
ifeq (,$(wildcard dchfev.f))
	wget https://netlib.org/slatec/pchip/dchfev.f
endif
ifeq (,$(wildcard dpchfd.f))
	wget https://netlib.org/slatec/pchip/dpchfd.f
endif
ifeq (,$(wildcard dchfdv.f))
	wget https://netlib.org/slatec/pchip/dchfdv.f
endif
ifeq (,$(wildcard xermsg.f))
	wget https://netlib.org/slatec/src/xermsg.f
endif
ifeq (,$(wildcard xerprn.f))
	wget https://netlib.org/slatec/src/xerprn.f
endif
ifeq (,$(wildcard xerhlt.f))
	wget https://netlib.org/slatec/src/xerhlt.f
endif
ifeq (,$(wildcard xersve.f))
	wget https://netlib.org/slatec/src/xersve.f
endif
ifeq (,$(wildcard xercnt.f))
	wget https://netlib.org/slatec/src/xercnt.f
endif
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	$(F77) -shared -o libpchip.so $(OBJ)
	install libpchip.so ../

clean:
	rm -f *.o *.so *~
