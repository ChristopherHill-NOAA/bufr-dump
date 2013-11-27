SHELL=/bin/sh

SRCS=	dumpmd.f
OBJS=	dumpmd.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#

FC =		ifort    
LIBS =          -L/nwprod/lib -lw3nco_v2.0.6_4 -lbufr_v10.2.5_4_64
#CHKOUT =        -ftrapuv  -check all  -fp-stack-check  -fstack-protector
DEBUG =         -g -traceback
FFLAGS =        -O2 $(DEBUG) $(CHKOUT)

CMD =		bufr_dumpmd

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS) 

install:
	mv $(CMD) ../../exec/

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile
