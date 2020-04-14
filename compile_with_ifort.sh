icc -c libc.c
ifort -fpp -warn all libc.o test_libc.f90 -o test_libc