SHELL=		/bin/sh
FC =            ifort
LIBS=           -L/nwprod/lib -lw3nco_v2.0.6_4 -lw3emc_v2.0.5_4 -lw3nco_v2.0.6_4 -lbufr_v10.2.5_4_64 -lsp_v2.0.1_4 -lbacio_v2.0.1_4
FFLAGS =        -O3 -g -traceback
LDFLAGS=
PROFLIB =

SRCS=		prepssmi.f 
OBJS=		prepssmi.o 
CMD=		prepobs_prepssmi

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

