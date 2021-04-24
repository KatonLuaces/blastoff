#!/bin/bash

./blastoff.native testlocal.bl > blastoff.ll \
    && llc -relocation-model=pic blastoff.ll > blastoff.s \
    &&  cc -o blastoff.exe blastoff.s graphblas.o -lgraphblas \
    && ./blastoff.exe
