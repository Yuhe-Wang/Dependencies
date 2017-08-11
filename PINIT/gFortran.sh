#!/bin/sh
mkdir ../Build/PINIT
gfortran PInterface.f penelope.f -shared -fPIC -m64 -o ../Build/PINIT/PINIT.so -O3 -fno-underscoring
