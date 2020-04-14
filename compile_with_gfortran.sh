gcc -c libc.c
gfortran-9 -cpp -Wall -no-pie -fPIC -fbounds-check libc.o test_libc.f90 -o test_libc