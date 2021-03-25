#!/bin/bash

make
./blastoff.native helloworld.bl > helloworld.ll
llc -relocation-model=pic helloworld.ll > helloworld.s
cc  -o  helloworld.exe  helloworld.s  graphblas.o
./helloworld.exe