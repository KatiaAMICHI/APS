#! /bin/bash

for i in `ls ../../../tests/aps1/*.aps`
do
	echo $i " -> "
	./toProlog $i
	echo -e
done
