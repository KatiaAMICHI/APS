#! /bin/bash

for i in `ls ../../../tests/aps3/*.aps`
do
	echo $i " -> "
	./../syntaxe/toProlog $i
  echo  " typage : "
  ./../syntaxe/toProlog $i| swipl -s ../typage/typage.pl -g main_stdin

	echo -e
done
