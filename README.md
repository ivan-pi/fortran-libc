# Fortran interface to the C standard library

Work in progress...

To compile with gfortran:
```
gcc -Wall -c libc.c -no-pie
gfortran -cpp -Wall -no-pie libc.o test_libc.f90 -o test_libc 
```

On Linux, the flag `-no-pie` is needed to generate a non-position independent executable. See the following discussions:
* [32-bit absolute addresses no longer allowed in x86-64 Linux?](https://stackoverflow.com/questions/43367427/32-bit-absolute-addresses-no-longer-allowed-in-x86-64-linux?noredirect=1&lq=1)
* [Nasm - Symbol `printf` causes overflow in R_X86_64_PC32 relocation](https://stackoverflow.com/questions/48071280/nasm-symbol-printf-causes-overflow-in-r-x86-64-pc32-relocation)


To compile with the Intel compilers:
```
icc -c libc.c
ifort -fpp -warn all libc.o test_libc.f90 -o test_libc
```

## Examples

See the file `test_libc.f90` for usage examples following those available at http://www.cplusplus.com/reference/ctime/

## Spurious behavior

* When compiled with gfortran, if `mktime` (or its Fortran wrapper) is called twice it somehow corrupts memory and issues a [double free or corruption error](https://jblevins.org/log/double-free). If the size of the `tm` struct is increased from 9 to at least 11 integers, then it works properly on both compilers. See also:
  - [Size of struct tm](https://stackoverflow.com/questions/13969210/size-of-struct-tm); The glibc version of struct tm has two additional fields `tm_gmtoff` and `tm_zone` for the seconds east of UTC and the timezone abbreviation, respectively.

* In some circumstances on the Fortran side the opaque pointers to the `time_t` and `struct tm` need to be declared as `target` in order to be able to call `c_loc` on the internal `type(c_ptr)` variable.
