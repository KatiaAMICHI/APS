#! /bin/bash

for i in `ls ../../../tests/aps2/*.aps`
do
	echo $i " -> "
	./eval `./toProlog $i`
	echo -e
done
