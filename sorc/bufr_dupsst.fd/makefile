SHELL=/bin/sh

SRCS=	dupsst.f
OBJS=	dupsst.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
FC =            ifort

LIBS =          -L/nwprod/lib -lw3nco_v2.0.6_4 -lw3emc_v2.0.5_4 -lbufr_v10.2.5_4_64
#CHKOUT =        -ftrapuv  -check all  -fp-stack-check  -fstack-protector
DEBUG =         -g -traceback
FFLAGS =        -O2 $(DEBUG) $(CHKOUT)
LDFLAGS =

CMD =		bufr_dupsst

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
