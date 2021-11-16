#!/bin/bash
#----- cd /change working directory
cd /User/homes/yluo/VI_gcc
#ulimit -c 0
#----- specify the R-Script, commandline argument $1 has precedence

# get the error messages in english
export LANG=en_US
# only one thread, no automatic distributed computing
export OMP_NUM_THREADS=1

RFILE=phenocam_ftp.R
if [ $# -ne 0 ]; then
  RFILE=$1
fi
#echo $RFILE
#echo Rout/`basename $RFILE .R`.rout
echo $RFILE
DATUM=$(date '+%d_%m_%Y_%Hh%Mm') 

FOLDER="${DATUM}_${LSB_JOBNAME}"
NUMPROC="$LSB_MCPU_HOSTS"

echo $DATUM

# creates new folder
mkdir /User/homes/yluo/Rout/$FOLDER 

#--------- execute R
#R CMD BATCH --vanilla $RFILE Rout/$DATUM.rout
#/usr/local/apps/R/R-2.15.1_ACML/bin/R CMD BATCH  --vanilla $RFILE /Net/Groups/BGI/scratch/bahrens/Rout/Rout_COMISSION/$DATUM/$DATUM.rout 
#/usr/local/apps/R/R-2.15.1_ACML/bin/R CMD BATCH  --vanilla "--args $FOLDER" $RFILE /Net/Groups/BGI/scratch/bahrens/Rout/Rout_COMISSION/$FOLDER/${LSB_JOBNAME}.rout
#/usr/local/apps/R/R-3.1.1/bin/R CMD BATCH  --vanilla "--args $FOLDER $LSB_DJOB_NUMPROC" $RFILE /Net/Groups/BGI/scratch/bahrens/Rout/Rout_JSM/$FOLDER/${LSB_JOBNAME}.rout
/usr/local/apps/R/R-3.2.0/bin/R CMD BATCH  --vanilla "--args $FOLDER $NUMPROC" $RFILE /User/homes/yluo/Rout/$FOLDER/${LSB_JOBNAME}.rout
#/usr/local/apps/R/R-2.15.1/bin/R
#/usr/local/apps/R/R-2.14.1/bin/R CMD BATCH --vanilla $RFILE Rout/`basename $RFILE .R`.rout
#/usr/local/apps/R/R-2.12.2/bin/R CMD BATCH --vanilla $RFILE Rout/`basename $RFILE .r`.rout
#R CMD BATCH --vanilla $RFILE Rout/`basename $RFILE .r`.rout