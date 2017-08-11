mkdir x64
gfortran PInterface.f penelope.f -shared -m64 -o x64\PINIT.dll -O3 -fno-underscoring -static-libgcc -static-libgfortran
