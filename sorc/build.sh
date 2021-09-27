#!/bin/sh
# build.sh
#   compilation wrapper for sorc/*.fd code packages
#
#   Usage:
#      [opt: debug=yes] build.sh [opt: *.fd package list] 
#
#        arguments: source directory list of packages to build
#                   blank  compiles all sorc/*.fd packages
#
#   importable var:  debug 
#      if debug is passed in w/ value 'yes', then DEBUG2 compiler options 
#       are enabled in the makefiles
# 
# modification history
#   1 Aug 2019 - original script
#  29 Aug 2019 - added optional debug importable env variable to enable 
#                  DEBUG2 compile options
#----
lab='build.sh'
usage="[optional: debug=yes] build.sh [optional: *.fd sorc subdir list (blank==all)]" 
[[ "$1" == '-h' ]] && { echo "$lab: USAGE: $usage" ; exit ; }

# check to see if DEBUG2 compile options are requested
debug=${debug:-''}                # default to not using DEBUG2 flags

echo "$lab: welcome to obsproc_dump sorc build script ($(date))"
[[ "$debug" == 'yes' ]] && echo "$lab:  -debug options enabled (debug='$debug')"

#echo "$lab: db exit" ; exit

#set -x
set -e    # fail if an error is hit so that errors do not go unnoticed

##  determine system/phase

module purge

##   On phase 3 with Lua Modules, loading prod_util without preloading 
##   dependent modules will fail.  This means that we need to know the 
##   system in order to be able to run the getsystems.pl utility so as 
##   to determine the system.
##   To overcome this circular logic, use hostname to do special loading 
##   of the prod_util module so as to run the getsystems.pl utility.

hname=$(hostname)
if [[ $hname =~ ^[vmp][0-9] ]] ; then # Dell-p3: venus mars pluto
  module load ips/18.0.1.163
  module load prod_util/1.1.0

else
  ## On non-phase 3 systems, can simply load prod_util directly

  module load prod_util               # non-Lua Modules system
fi # if hname starts w/ [vmp][digit]

sys_tp=$(getsystem.pl -tp)
echo "$lab:  -running on $sys_tp"

#echo "$lab: db exit" ; exit

module purge

case $sys_tp in
 Cray-XC40)
   module load PrgEnv-intel;
   module load craype-sandybridge;
   module swap intel/16.3.210;
   lib_build="intel";
   export FC=ftn;
   ;;
 IBM-p1|IBM-p2)
   module load ics/12.1;
   module load ibmpe;
   ;;
 Dell-p3)
   module load ips/18.0.1.163 ;    # req'd for bufr
   module load impi/18.0.1    ;    # req'd for w3emc
   ;;
 *) echo "$lab: unexpected system.  Update for $sys_tp";
    echo "$lab: exiting" ; exit ;;
esac

source ./load_libs.rc  # use modules to set library related environment variables

echo ; module list

if [ $# -eq 0 ]; then
  dir_list=*.fd
else
  dir_list=$*
fi

echo ; echo "$lab: list of dirs to build..."
echo $dir_list


# set DEBUG2 compiler options
# -extra option in dcodquikscat makefile
# ---
dbstr='-ftrapuv  -check all  -fp-stack-check  -fstack-protector'
dbstr2='-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector'

clobber=${clobber:-clobber_yes}  # user can override the default of running "make clobber"
set +x                      # jaw db
for sdir in $dir_list; do

 echo 
 dir=${sdir%\/}  # chop trailing slash if necessary
 echo "$lab: ------- making ${dir%\.fd*}"

 DEBUG2=''
 if [ "$debug" = 'yes' ] ; then 
   echo "$lab:  -compiler DEBUG2 options are ENABLED"
   if [[ "$sdir" =~ dcodquikscat ]] ; then 
     export DEBUG2="$dbstr2"
   else 
     export DEBUG2="$dbstr"
   fi # sdir = dcodquikscat
   echo "$lab:  -DEBUG2='$DEBUG2'"
 fi # debug = yes

#echo "$lab: db continue w/o compiling" 
#continue  # jaw db

 cd $dir
 [ $clobber != clobber_no ]  && { echo "$lab:  -clobbering:" ; make clobber ; }
 echo ; echo "$lab:  -compiling:"
 if [ $sys_tp = Cray-XC40 ]; then
   make FC=$FC
 else
   make
 fi
 ###touch *
 echo ; echo "$lab:  -results:"
 ls -l
 cd ..
done

echo ; echo "$lab: end of script ($(date))"

