SHELL=/bin/sh
SRCS =  supertmi.f90

OBJS = supertmi.o


#
# *****************************************************************
#

CF =            ifort
LIBS=           -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbufr_4_64 -lbacio_4
FFLAGS =        -O3 -g -traceback
LDFLAGS=
PROFLIB =

CMD = bufr_supertmi

all:  $(CMD)

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)


supertmi.o : supertmi.f90
	$(CF) $(FFLAGS) -c $(*).f90

