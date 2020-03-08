#!/bin/sh

cd '/Applications/actr6/' && echo $'\ec'
open environment/Start\ Environment\ OSX.app
echo starting environment GUI...
echo "loading ACT-R in 5 sec..."
sleep 5
echo now loading ACT-R...

ccl --load load.lisp
