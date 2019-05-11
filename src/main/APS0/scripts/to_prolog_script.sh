#! /bin/bash

for i in `ls ../../../tests/aps0/*.aps`
do
	echo $i " -> "
	./../syntaxe/toProlog $i
	echo -e
done
