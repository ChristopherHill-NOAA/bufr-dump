SHELL=		/bin/sh
FC =            ifort
LIBS=           -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lw3nco_4 -lbufr_4_64 -lsp_4 -lbacio_4
FFLAGS =        -O3 -g -traceback
LDFLAGS=
PROFLIB =

SRCS=		prepssmi.f 
OBJS=		prepssmi.o 
CMD=		prepobs_prepssmi

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

