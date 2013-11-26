SHELL=/bin/sh
# makefile (bufr_geofil) - 22 Jan 2013
#
# 14 Jan 2013  JWhiting  cp'd fr IBM/AIX ver of 18 Dec 2008
#               config'd for use on WCOSS (ifort cmplr, new libs)
#                 parameterized cmpl debug options (-g, -traceback)
#                 set optimization to default (-O2)
#                 rm'd unneeded compiler options (list, source, etc)
#                 set to use default variable sizes (-i4, -r4)
#                 added developmental cmpl options (CHKOUT)
# 22 Jan 2013  JWhiting  added -convert big_endian option, necessary for
#               proper reading of big endian formatted geographic mask
#               files.
# ----
#
SRCS=	geofil.f

OBJS=	geofil.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
FC =            ifort
LIBS =          -L/nwprod/lib -lw3nco_4 -lbufr_v10.2.3_4_64
#CHKOUT =        -ftrapuv  -check all  -fp-stack-check  -fstack-protector
DEBUG =         -g -traceback
FFLAGS =        -O2 -convert big_endian $(DEBUG) $(CHKOUT)
LDFLAGS =

CMD =		bufr_geofil 

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
