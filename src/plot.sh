#!/bin/bash

moduleName=""
buildName="auto-cmg-plot"

ERR='\033[0;31m' #RED
END='\033[0;32m' #GREEN
LOG='\033[0;34m' #BLUE
DEF='\033[0m'

if [[ ($# -eq 0) && ("$moduleName" = "") ]]; then
    echo -e "${ERR}Error:$DEF Enter module name to build. (Ex.: ./plot.sh Synthesis/Render) "
    echo "Or change moduleName value at line 3 with default module name."
    exit 1
fi

hash octave &>/dev/null
if [ $? -ne 0 ]
then
    echo "[error] 'octave' command not found on the system. It is required to plot!" >&2
    echo "I AM INSTALLING IT FOR YOU <3"
    sudo apt install octave
fi

if [[ $# -ge 1 ]]; then
    moduleName=$1
fi

if [[ $# -ge 2 ]]; then
    buildName=$2
fi

buildCmd="clm $moduleName -o $buildName"

if test -f $buildName; then
    echo -e "${LOG}Removing old builds...${DEF}"
    rm $buildName
fi

echo -e "${LOG}Building $moduleName...${DEF}"
eval $buildCmd

if !(test -f $buildName); then
    echo -e "${ERR}Error:$DEF Build Failed - Executable not found."
    exit 1
fi

echo -e "${LOG}Running...${DEF}"
output=$(./$buildName)

echo -e "${LOG}Plotting...${DEF}"
octave --eval='plot('$output'), x=input("Press Enter to close.")'

echo -e "${END}Done${DEF}"
