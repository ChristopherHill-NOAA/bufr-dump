SHELL=		/bin/sh
LIBS =		-L/nwprod/lib -lw3nco_v2.0.6_4 -lw3emc_v2.0.5_4 -lbufr_v10.2.5_4_64
FFLAGS=		-O3 -g -traceback 
LDFLAGS= 

SRCS=	dcodwindsat.f 

OBJS=   dcodwindsat.o  

FC =	ifort    

CMD =  bufr_dcodwindsat

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

