SHELL=		/bin/sh

FC =            ifort
LIBS=           -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbufr_v10.2.3_4_64 -lbacio_4
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

