#! /bin/bash

for i in `ls ../../../tests/aps2/*.aps`
do
  echo $i " -> "
	./../syntaxe/eval $i
	echo -e
done
