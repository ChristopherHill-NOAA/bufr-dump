SHELL=		/bin/sh
LIBS =		-L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbufr_4_64  
FFLAGS=		-O3 -g -traceback 
LDFLAGS= 

SRCS=	dcodwindsat.f 

OBJS=   dcodwindsat.o  

FC =	ifort    

CMD =  bufr_dcodwindsat

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

