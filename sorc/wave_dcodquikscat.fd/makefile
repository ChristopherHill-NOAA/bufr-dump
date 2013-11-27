SHELL=		/bin/sh

FC =            ifort
LIBS=           -L/nwprod/lib -lw3nco_v2.0.6_4 -lw3emc_v2.0.5_4 -lbufr_v10.2.5_4_64 -lbacio_v2.0.1_4
#DEBUG =         -g -traceback
FFLAGS =        -O2 -convert big_endian ${DEBUG} 
LDFLAGS=
PROFLIB =

SRCS=	dcodquikscat.f 

OBJS=   dcodquikscat.o  

CMD =  wave_dcodquikscat

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

