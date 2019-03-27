#! /bin/bash

for i in `ls ../../tests/aps0/*.aps`
do
	echo $i " -> "
	./toProlog $i
	echo -e
done
