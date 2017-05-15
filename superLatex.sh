#!/bin/sh

# "${1%.*}" extract filename
# "${1##*.}"  extract extension

 pdflatex $1

 biber "${1%.*}"

 pdflatex $1

 pdflatex $1
