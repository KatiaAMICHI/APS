#! /bin/bash

for i in `ls ../../../tests/aps1/*.aps`
do
  echo $i " -> "
	./../syntaxe/eval $i
	echo -e
done
