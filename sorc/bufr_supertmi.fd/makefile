SHELL=/bin/sh
SRCS =  supertmi.f90

OBJS = supertmi.o


#
# *****************************************************************
#

CF =            ifort
LIBS=           -L/nwprod/lib -lw3nco_v2.0.6_4 -lw3emc_v2.0.5_4 -lbufr_v10.2.5_4_64 -lbacio_v2.0.1_4
FFLAGS =        -O3 -g -traceback
LDFLAGS=
PROFLIB =

CMD = bufr_supertmi

all:  $(CMD)

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)


supertmi.o : supertmi.f90
	$(CF) $(FFLAGS) -c $(*).f90

