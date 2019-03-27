#! /bin/bash

for i in `ls ../../../tests/aps1/*.aps`
do
	echo $i " -> "
	./../syntaxe/toProlog $i
  echo  " typage : "
  ./../syntaxe/toProlog $i| swipl -s typage.pl -g main_stdin

	echo -e
done
