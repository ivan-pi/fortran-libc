gcc -c libc.c
gfortran -cpp -Wall -no-pie -fbounds-check libc.o test_libc.f90 -o test_libc