#!/bin/bash
IMAGES=$(ls -d */)
BASEDIR=$(pwd)
if [ $# -eq 0 ] || [ $# -ge 3 ]; then
	echo "Illegal number of parameters"
	echo "Usage:"
	echo "\tbuild_images TAGNAME IMAGE"
	echo "where TAGNAME will be used to tag the images uploaded to Docker Hub."
	exit 1
else
	TAG=$1
fi
if [ $# -eq 2 ]; then
	IMAGES=($2)
fi
# for I in $IMAGES; do
for I in ${IMAGES[@]}; do
	# Make all letters lowercase and remove the ending babcslash
	IMG=$(echo $I | tr '[:upper:]' '[:lower:]' | tr -d '[:punct:]')
	DIR=$BASEDIR/$I
	cd $DIR
	if [ -f $DIR/Dockerfile ]; then
		date > marker
		echo "Building $IMG"
		echo -e "\tExecuting docker build -t qbioturin/orca-$IMG:$TAG ."
		docker build -t qbioturin/orca-$IMG:$TAG .
		if [[ $? -ne 0 ]]; then
			exit 0
		fi
		rm marker
	 	echo "Uploading $IMG"
	        	echo -e "\tExecuting docker push qbioturin/orca-$IMG:$TAG"
	 	docker push qbioturin/orca-$IMG:$TAG
	 	if [[ $? -ne 0 ]]; then
	 		exit 0
	 	fi
	else
		echo "Dockerfile missing for image qbioturin/orca-$IMG"
	fi
	cd $BASEDIR
done
exit 0
