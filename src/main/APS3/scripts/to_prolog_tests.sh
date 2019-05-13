#! /bin/bash

for i in `ls ../../../tests/aps3/*.aps`
do
	echo $i " -> "
	./../syntaxe/toProlog $i
	echo -e
done
