#!/bin/bash
##################################################################################
# 1.1
# path setup below is for Mac OS
# adding R paths
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:/Library/Frameworks/R.framework/Resources
# adding GRASS bin paths
grassApp=$(ls /Applications | grep GRASS)
grassVersion=$(ls /Applications | grep GRASS | awk -F'[.-]' '{print $2"."$3}')
grassCMD=$(ls /Applications/"$grassApp"/Contents/Resources/bin | grep grass)
export PATH=$PATH:/Applications/"$grassApp"/Contents/Resources/bin:/Applications/"$grassApp"/Contents/Resources/scripts:/etc:/usr/lib
# adding GRASS environment variables: GISBASE_USER and GISBASE_SYSTEM in Grass.sh
export PATH=$PATH:~/Library/GRASS/"$grassVersion"/Modules/bin:~/Library/GRASS/"$grassVersion"/Modules/scripts
# adding GDAL and EPSG code lookup paths (Running gdal-config --datadir shows where GDAL searches for gcs.csv)
export GDAL_DATA=/Applications/"$grassApp"/Contents/Resources/share/gdal
GITHUBLIBRARIES="https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/libraries"
##################################################################################
### setup GRASS dataset
mkdir delete_me
PROJDIR='./delete_me' # full path to the project location;
EPSGCODE='EPSG:26918' # need to (manually) lookup the EPSG code for NAD83 UTM ##N for the catchment
RESOLUTION=30 #spatial resolution (meters) of the grids
RHESSysNAME='delete_me' # e.g., rhessys_baisman10m
GISDBASE="$PROJDIR"/grass_dataset
LOCATION_NAME="$RHESSysNAME"
LOCATION="$GISDBASE"/$LOCATION_NAME
MAPSET=PERMANENT
### ... create grass database
mkdir "$PROJDIR"/grass_dataset # does not overwrite
$grassCMD -c $EPSGCODE -e "$LOCATION" 
##################################################################################
### catchment outlet
gageLat='39.44294444' # catchment outlet WSG84 Lat (decimal degree)
gageLong='-76.7834167' # catchment outlet WSG84 Long (decimal degree; includes the negative sign if applied)
eval $($grassCMD "$LOCATION"/$MAPSET --exec m.proj -i coordinates=$gageLong,$gageLat separator=space | awk '{print "xyCoord=" $1 "," $2}')
echo $xyCoord | $grassCMD "$LOCATION"/"$MAPSET" --exec v.in.ascii in=- out=outlet x=1 y=2 separator=, --overwrite
##################################################################################
echo "it works!"
echo "grassApp" "$grassApp"
echo "grassVersion" "$grassVersion"
echo "grassCMD" "$grassCMD"
rm -rf "$PROJDIR"
##################################################################################
echo "installed.packages()[,c(1,3)]" | R --slave | grep sp | awk 'END{print (NR>0? "yes, you have installed sp" : "no, you have not installed sp") }'
echo "installed.packages()[,c(1,3)]" | R --slave | grep xml | awk 'END{print (NR>0? "yes, you have installed xml" : "no, you have not installed xml") }'
echo "installed.packages()[,c(1,3)]" | R --slave | grep rgdal | awk 'END{print (NR>0? "yes, you have installed rgdal" : "no, you have not installed rgdal") }'
echo "installed.packages()[,c(1,3)]" | R --slave | grep SQLite | awk 'END{print (NR>0? "yes, you have installed SQLite" : "no, you have not installed SQLite") }'
#echo "installed.packages()[,c(1,3)]" | R --slave | grep sf | awk 'END{print (NR>0? "yes, you have installed sf" : "no, you have not installed sf") }'
#echo "installed.packages()[,c(1,3)]" | R --slave | grep stars | awk 'END{print (NR>0? "yes, you have installed stars" : "no, you have not installed stars") }'
