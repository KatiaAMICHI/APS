#! /bin/bash

for i in `ls aps0/*.aps` 
do 
	./toProlog $i
done 
