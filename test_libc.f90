module libc
    
    use, intrinsic :: iso_c_binding
    implicit none

    public :: cmalloc, cfree, crealloc, ccalloc

    interface
        subroutine set_errno(n) bind(c,name='set_errno')
            import c_int
            integer(c_int), intent(in), value :: n
        end subroutine
        integer(c_int) function get_errno() bind(c,name='get_errno')
            import c_int
        end function
        type(c_ptr) function strerror(errnum) bind(c,name='strerror')
            import c_ptr, c_int
            integer(c_int), intent(in), value :: errnum
        end function
        subroutine perror(str) bind(c,name='perror')
            import c_char
            character(kind=c_char) :: str(*)
        end subroutine
        integer(c_int) function test_strerror() bind(c,name='test_strerror')
            import c_int
        end function

    end interface

    !
    ! <stdlib.h>
    !
    interface

        !-----------------------------------
        ! Pseudo-random sequence generation
        !-----------------------------------
        
        ! Generate random number
        ! int rand (void);
        ! integer(c_int) function rand() bind(c,name='rand')
        !     import c_int
        ! end function

        ! Initialize random number generator
        ! void srand (unsigned int seed);
        ! subroutine srand(seed) bind(c,name='srand')
        !     import c_int
        !     integer(c_int), value :: seed
        !         !! An integer value to be used as seed by the pseudo-random number generator algorithm.
        ! end subroutine

        ! Maximum value returned by rand
        ! integer(c_int) function get_rand_max() bind(c,name='get_rand_max')
        !     import c_int
        ! end function


        !---------------------------
        ! Dynamic memory management
        !---------------------------

        ! Allocate memory block
        ! void* malloc (size_t size);
        type(c_ptr) function cmalloc(size) bind(c,name='malloc')
            import c_ptr, c_size_t
            integer(c_size_t), intent(in), value :: size
        end function

        ! Deallocate memory block
        ! void free (void* ptr);
        subroutine cfree(ptr) bind(c,name='free')
            import c_ptr
            type(c_ptr), value :: ptr
        end subroutine

        ! Reallocate memory block
        ! void* realloc (void* ptr, size_t size);
        type(c_ptr) function crealloc(ptr,new_size) bind(c,name='realloc')
            import c_ptr, c_size_t
            type(c_ptr), value :: ptr
            integer(c_size_t), intent(in), value :: new_size
        end function

        ! Allocate and zero-initialize array
        ! void* calloc (size_t num, size_t size);
        type(c_ptr) function ccalloc(num,size) bind(c,name='calloc')
            import c_ptr, c_size_t
            integer(c_size_t), intent(in), value :: num !! number of objects
            integer(c_size_t), intent(in), value :: size !! size of each object (in bytes)
        end function

    end interface

    type, bind(c) :: tm
        integer(c_int) :: tm_sec
        integer(c_int) :: tm_min
        integer(c_int) :: tm_hour
        integer(c_int) :: tm_mday
        integer(c_int) :: tm_mon
        integer(c_int) :: tm_year
        integer(c_int) :: tm_wday
        integer(c_int) :: tm_yday
        integer(c_int) :: tm_isdst
    end type

    type, bind(c) :: ctm_t
        ! integer(c_int) :: tm_sec
        ! integer(c_int) :: tm_min
        ! integer(c_int) :: tm_hour
        ! integer(c_int) :: tm_mday
        ! integer(c_int) :: tm_mon
        ! integer(c_int) :: tm_year
        ! integer(c_int) :: tm_wday
        ! integer(c_int) :: tm_yday
        ! integer(c_int) :: tm_isdst
        type(c_ptr) :: ptr = c_null_ptr
    ! contains
    !     final :: destroy_ctm
    end type

    type, bind(c) :: ctime_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    type, bind(c) :: cclock_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !
    ! <time.h>
    !
    interface

        !-------------------
        ! Time manipulation
        !-------------------

        ! Clock program
        ! clock_t clock (void);
        function clock() result(res) bind(c,name='clock')
            import c_ptr
            type(c_ptr) :: res
        end function
        function fclock() result(res) bind(c,name='clock')
            import c_ptr, cclock_t
            type(cclock_t) :: res
        end function

        ! Clock ticks per second
        ! This macro evaluates to an expression of type clock_t.
        function get_clocks_per_sec() result(res) bind(c,name='get_clocks_per_sec')
            import c_ptr
            type(c_ptr) :: res
        end function


        ! Return difference between two times
        ! double difftime (time_t end, time_t beginning);
        function difftime(end,beginning) result(res) bind(c,name='difftime')
            import c_double, c_ptr
            type(c_ptr), value :: end
                !! Higher bound of the time interval whose length is calculated.
            type(c_ptr), value :: beginning
                !! Lower bound of the time interval whose length is calculated.
                !! If this describes a time point later than end, the result is negative.
            real(c_double) :: res
                !! The result of (end-beginning) in seconds as a floating-point value of type double.
        end function
        function fdifftime(end,beginning) result(res) bind(c,name='difftime')
            import c_double, ctime_t, c_ptr
            type(ctime_t), value :: end
                !! Higher bound of the time interval whose length is calculated.
            type(ctime_t), value :: beginning
                !! Lower bound of the time interval whose length is calculated.
                !! If this describes a time point later than end, the result is negative.
            real(c_double) :: res
                !! The result of (end-beginning) in seconds as a floating-point value of type double.
        end function

        ! Convert tm structure to time_t
        ! time_t mktime (struct tm * timeptr);
        type(c_ptr) function mktime(timeptr) bind(c,name='mktime')
            import c_ptr
            type(c_ptr), value :: timeptr
                !! Pointer to a tm structure that contains a calendar time broken down into its components
        end function
        type(ctime_t) function fmktime(timeptr) bind(c,name='mktime')
            import ctime_t, ctm_t
            type(ctm_t), value :: timeptr
                !! Pointer to a tm structure that contains a calendar time broken down into its components
        end function

        ! Get current time
        ! time_t time (time_t* timer);
        type(c_ptr) function time(timer) bind(c,name='time')
            import c_ptr
            type(c_ptr), value :: timer
        end function
        type(ctime_t) function ftime(timer) bind(c,name='time')
            import ctime_t
            type(ctime_t), optional :: timer
        end function

        !-----------------
        ! Time Conversion
        !-----------------

        ! Convert tm structure to string
        ! char* asctime (const struct tm * timeptr);
        function asctime(timeptr) result(carr) bind(c,name='asctime')
            import c_ptr
            type(c_ptr), value :: timeptr
            type(c_ptr) :: carr
        end function

        ! Convert time_t to tm as UTC time
        ! struct tm * gmtime (const time_t * timer);
        type(c_ptr) function gmtime(timer) bind(c,name='gmtime')
            import c_ptr
            type(c_ptr), intent(in), value :: timer
        end function
        type(ctm_t) function fgmtime(timer) bind(c,name='gmtime')
            import c_ptr, ctime_t, ctm_t
            type(ctime_t) :: timer
        end function

        ! Convert time_t to tm as local time
        ! struct tm * localtime (const time_t * timer)
        type(c_ptr) function localtime(timer) bind(c,name='localtime')
            import c_ptr
            type(c_ptr), intent(in), value :: timer
        end function
        type(ctm_t) function flocaltime(timer) bind(c,name='localtime')
            import c_ptr, ctime_t, ctm_t
            type(ctime_t) :: timer
        end function

        ! Format time as string
        ! size_t strftime (char* ptr, size_t maxsize, const char* format,
        !         const struct tm* timeptr );
        function strftime(ptr,maxsize,format,timeptr) result(res) bind(c,name='strftime')
            import c_ptr, c_size_t, c_char
            character(kind=c_char), intent(out) :: ptr(*) 
                !! Pointer to the destination array where the resulting C string is copied.
            integer(c_size_t), value :: maxsize
                !! Maximum number of characters to be copied to ptr, including the terminating null-character.
            character(kind=c_char), intent(in) :: format(*)
                !! C string containg any combination of regular characters and special format specifiers.
            type(c_ptr), intent(in), value :: timeptr
                !! Pointer to a tm structure that contains a calendar time broken down into its components.
            integer(c_size_t) :: res
        end function

        ! The strptime() function converts the character string pointed to by buf to values which are stored 
        ! in the tm structure pointed to by tm, using the format specified by format. 
        !
        ! char *strptime(const char *buf, const char *format, struct tm *tm);
        function strptime(buffer,format,tm) result(res) bind(c,name='strptime')
            import c_ptr, c_char
            character(kind=c_char), intent(in) :: buffer(*)
            character(kind=c_char), intent(in) :: format(*)
            type(c_ptr), value :: tm
            type(c_ptr) :: res
        end function

    end interface

contains

    function fstrerror(errnum) result(res)
        integer(c_int), intent(in) :: errnum
        interface
            ! Get string length
            ! size_t strlen ( const char * str );
            function strlen(ptr) result(res) bind(c,name='strlen')
                import c_size_t, c_ptr
                type(c_ptr), value :: ptr !! C string.
                integer(c_size_t) :: res !! The length of string. 
            end function
        end interface
        character(len=1,kind=c_char), pointer :: f_str(:)
        character(len=:,kind=c_char), allocatable :: res
        integer(c_int) :: len
        type(c_ptr) :: c_str
        integer(c_int) :: i

        c_str = strerror(errnum)
        len = int(strlen(c_str),c_int)
        allocate(f_str(int(len,c_int)))

        call c_f_pointer(c_str,f_str,[len])
        allocate(character(kind=c_char,len=len) :: res)
        do i = 1, len
            res(i:i) = f_str(i)
        end do
    end function

    function fasctime(timeptr) result(res)
        type(ctm_t) :: timeptr
        type(c_ptr) :: c_string
        character(kind=c_char,len=26), pointer :: f_string
        character(kind=c_char,len=:), allocatable :: res

        c_string = asctime(timeptr%ptr)
        call c_f_pointer(c_string,f_string)
        res = f_string(1:24)
    end function


!     function cmktime(timeptr) result(res)
!         type(ctm_t), intent(in) :: timeptr
!         type(ctime_t) :: res
!         res%ptr = mktime(timeptr%ptr)
!     end function

!     function cctime(timer) result(res)
!         type(ctime_t), optional :: timer
!         type(ctime_t) :: res
!         if (present(timer)) then
!             res%ptr = time(timer%ptr) 
!         else
!             res%ptr = time(c_null_ptr)
!         end if
!     end function

!     subroutine set(tm,hour,min,sec,year,mon,mday)
!         type(ctm_t), intent(inout) :: tm
!         integer(c_int), intent(in) :: hour,min,sec,year,mon,mday
!         interface
!             subroutine set_tm(ptr,hour,min,sec,year,mon,mday) bind(c,name='set_tm')
!                 import c_ptr, c_int
!                 type(c_ptr), value :: ptr
!                 integer(c_int), intent(in) :: hour,min,sec,year,mon,mday
!             end subroutine
!         end interface
!         !tm%ptr = cmalloc(c_sizeof(hour)*9)
!         call set_tm(tm%ptr,hour,min,sec,year,mon,mday)
!     end subroutine

!     ! subroutine destroy_ctm(this)
!     !     type(ctm_t) :: this

!     function cdifftime(end,beginning) result(res)
!         type(ctime_t), intent(in) :: end, beginning
!         real(c_double) :: res
!         res = difftime(end%ptr,beginning%ptr)
!     end function

!     function casctime(tm) result(res)
!         type(ctm_t), intent(in) :: tm
!         character(len=25), pointer :: f_string
!         character(len=24) :: res
!         type(c_ptr) :: c_string

!         c_string = asctime(tm%ptr)
!         call c_f_pointer(c_string,f_string)
!         res = f_string(1:24)
!     end function

! #if 0
!     function cclocaltime(time) result(res)
!         type(ctime_t) :: time
!         type(ctm_t) :: res
!         res%ptr = localtime(c_loc(time%ptr))
!         ! call c_f_pointer(res%ptr,res%raw,[9])
!     end function

! #endif
    subroutine clock_example

        type(cclock_t), target :: t1, t2, t3
        integer(c_int) :: f
        integer(c_int), pointer :: ft1, ft2, fclocks_per_sec

        write(*,*) "clock example"

        t1%ptr = clock()
        write(*,*) "Calculating..."
        f = frequency_of_primes(99999)
        write(*,*) "The number of primes lower than 100,000 is: ", f
        t2%ptr = clock()
        t3%ptr = get_clocks_per_sec()
        call c_f_pointer(c_loc(t1%ptr),ft1)
        call c_f_pointer(c_loc(t2%ptr),ft2)
        call c_f_pointer(c_loc(t3%ptr),fclocks_per_sec)
        print *, "clocks_per_sec = ", fclocks_per_sec
        write(*,*) "It took me ",ft2-ft1," clicks (",real(ft2-ft1,c_double)/real(fclocks_per_sec,c_double)," seconds)"
    

        t1 = fclock()
        write(*,*) "Calculating..."
        f = frequency_of_primes(99999)
        write(*,*) "The number of primes lower than 100,000 is: ", f
        t2 = fclock()
        t3%ptr = get_clocks_per_sec()
        call c_f_pointer(c_loc(t1%ptr),ft1)
        call c_f_pointer(c_loc(t2%ptr),ft2)
        call c_f_pointer(c_loc(t3%ptr),fclocks_per_sec)
        print *, "clocks_per_sec = ", fclocks_per_sec
        write(*,*) "It took me ",ft2-ft1," clicks (",real(ft2-ft1,c_double)/real(fclocks_per_sec,c_double)," seconds)"
    
    contains

        function frequency_of_primes(n) result(freq)
            integer(c_int), intent(in) :: n
            integer(c_int) :: i,j,freq
            real(c_double) :: is
            freq = n - 1
            do i = 2, n
                is = sqrt(real(i,c_double))
                do j = int(is), 2, -1
                    if (mod(i,j) == 0) then
                        freq = freq - 1
                        exit
                    end if
                end do
            end do
        end function

    end subroutine


    subroutine difftime_example
        type(ctime_t), target :: now
        type(ctm_t) :: newyear
        real(c_double) :: seconds
        integer(c_int), pointer :: itm(:)
        
        write(*,*) "difftime example"

        now%ptr = time(now%ptr)
        newyear%ptr = localtime(c_loc(now%ptr))
        call c_f_pointer(newyear%ptr,itm,[14])

        ! sec, min, hour, mday, mon, year, wday, yday, isdst 
        print *, itm
        itm(3) = 0 ! hour 
        itm(2) = 0 ! min
        itm(1) = 0 ! sec
        itm(5) = 0 ! mon
        itm(4) = 1 ! mday
        print *, itm

        ! ny%ptr = mktime(newyear%ptr)
        seconds = difftime(now%ptr,mktime(newyear%ptr))

        write(*,*) seconds, " seconds since new year in the current timezone"
        write(*,*) seconds/(24.*3600.), " days since new year in the current timezone"

        seconds = fdifftime(now,fmktime(newyear))
        write(*,*) seconds, " seconds since new year in the current timezone"
        write(*,*) seconds/(24.*3600.), " days since new year in the current timezone"
    end subroutine

    subroutine mktime_example
        use iso_fortran_env, only: output_unit, input_unit
        type(ctime_t), target :: rawtime, thattime
        type(ctm_t) :: timeinfo
        integer(c_int) :: year, month, day
        integer(c_int), pointer :: itm(:)
        character(len=9) :: weekday(0:6) = [character(len=9) :: "Sunday","Monday","Tuesday",&
            "Wednesday","Thursday","Friday","Saturday"]

        write(output_unit,*) "mktime example"

        write(output_unit,"(A)",advance='no') "Enter year: "
        read(input_unit,*) year
        write(output_unit,"(A)",advance='no') "Enter month: "
        read(input_unit,*) month
        write(output_unit,"(A)",advance='no') "Enter day: "
        read(input_unit,*) day

        rawtime%ptr = time(rawtime%ptr)
        timeinfo%ptr = localtime(c_loc(rawtime%ptr))
        call c_f_pointer(timeinfo%ptr,itm,[9])
        itm(6) = year - 1900
        itm(5) = month - 1
        itm(4) = day

        thattime%ptr = mktime(timeinfo%ptr)

        write(output_unit,"(A,A)") "That day is a ", weekday(itm(7))


        rawtime = ftime(rawtime)
        timeinfo = flocaltime(rawtime)
        call c_f_pointer(timeinfo%ptr,itm,[9])
        itm(6) = year - 1900
        itm(5) = month - 1
        itm(4) = day

        thattime = fmktime(timeinfo)
        write(output_unit,"(A,A)") "That day is a ", weekday(itm(7))

    end subroutine


    subroutine time_example
        type(ctime_t), target :: timer, thattime
        type(ctime_t), target :: ftimer
        type(ctm_t), target :: y2k
        integer(c_int), pointer :: itm(:)
        real(c_double) :: seconds

        write(*,*) "time example"

        allocate(itm(11))
        itm = 0
        itm(4) = 1   ! mday
        itm(6) = 100 ! year
        itm(5) = 0   ! mon

        print *, itm
        y2k%ptr = c_loc(itm)
        ! stop

        timer%ptr = time(timer%ptr)

        thattime%ptr = mktime(y2k%ptr)
        seconds = difftime(timer%ptr,thattime%ptr)
        write(*,*) seconds, " seconds since January 1, 2000 in the current timezone"
        write(*,*) seconds/(24.*3600.), " days since January 1, 2000 in the current timezone"
        
        thattime%ptr = mktime(y2k%ptr)
        ! seconds = fdifftime(ftimer,thattime)
        ftimer= ftime()
        seconds = fdifftime(ftimer,thattime)
        ! seconds = fdifftime(ftimer,fmktime(y2k)) ! works with ifort
        write(*,*) seconds, " seconds since January 1, 2000 in the current timezone"
        write(*,*) seconds/(24.*3600.), " days since January 1, 2000 in the current timezone"
    end subroutine

    subroutine asctime_example
        type(ctime_t), target :: rawtime
        type(ctm_t) :: timeinfo

        character(len=25), pointer :: fstring
        type(c_ptr) :: c_string

        write(*,*) new_line('a')//"asctime_example"

        rawtime%ptr = time(rawtime%ptr)
        timeinfo%ptr = localtime(c_loc(rawtime%ptr))

        c_string = asctime(timeinfo%ptr)
        call c_f_pointer(c_string,fstring)
        write(*,*) "The current date/time is: ", fstring(1:24)

        rawtime = ftime(rawtime)
        timeinfo = flocaltime(rawtime)

        write(*,*) "The current date/time is: ", fasctime(timeinfo)
    end subroutine


    subroutine gmtime_example
        type(ctime_t), target :: rawtime
        type(ctime_t) :: frawtime
        type(ctm_t) :: ptm, fptm
        integer(c_int), pointer :: itm(:)
        integer(c_int), parameter :: mst = -7, utc = 0, cct = 8

        write(*,*) new_line('a')//"gmtime_example"

        rawtime%ptr = time(rawtime%ptr)
        frawtime = ftime(frawtime)

        ptm%ptr = gmtime(c_loc(rawtime%ptr))
        fptm = fgmtime(frawtime)

        call c_f_pointer(ptm%ptr,itm,[11])
        write(*,*) "Current time around the world:"
        write(*,"(A,I2,A1,I2)") "Phoenix, AZ (U.S.) :  ", mod(itm(3)+MST,24), ":", itm(2)
        write(*,"(A,I2,A1,I2)") "Reykjavik (Iceland) : ", mod(itm(3)+UTC,24), ":", itm(2)
        write(*,"(A,I2,A1,I2)") "Beijing (China) :     ", mod(itm(3)+CCT,24), ":", itm(2)

        call c_f_pointer(fptm%ptr,itm,[11])
        write(*,*) "Current time around the world:"
        write(*,"(A,I2,A1,I2)") "Phoenix, AZ (U.S.) :  ", mod(itm(3)+MST,24), ":", itm(2)
        write(*,"(A,I2,A1,I2)") "Reykjavik (Iceland) : ", mod(itm(3)+UTC,24), ":", itm(2)
        write(*,"(A,I2,A1,I2)") "Beijing (China) :     ", mod(itm(3)+CCT,24), ":", itm(2)
    end subroutine

    subroutine localtime_example
        type(ctime_t), target :: rawtime
        type(ctm_t) :: timeinfo

        character(len=25), pointer :: fstring
        type(c_ptr) :: c_string

        write(*,*) new_line('a')//"localtime_example"

        rawtime%ptr = time(rawtime%ptr)
        timeinfo%ptr = localtime(c_loc(rawtime%ptr))

        c_string = asctime(timeinfo%ptr)
        call c_f_pointer(c_string,fstring)
        write(*,*) "Current local time and date: ", fstring(1:24)

        rawtime = ftime(rawtime)
        timeinfo = flocaltime(rawtime)

        ! c_string = asctime(timeinfo%ptr)
        ! call c_f_pointer(c_string,fstring)
        write(*,*) "Current local time and date: ", fasctime(timeinfo)
    end subroutine

    subroutine strftime_example
        type(ctime_t), target :: rawtime
        type(ctm_t) :: timeinfo

        character(len=80) :: buffer
        integer(c_size_t) :: ret

        write(*,*)  new_line('a')//"strftime_example"

        rawtime%ptr = time(rawtime%ptr)
        timeinfo%ptr = localtime(c_loc(rawtime%ptr))
        ! timeinfo%ptr = localtime(rawtime%ptr)
        ret = strftime(buffer,80_c_size_t,"Now it's %I:%M%p."//c_null_char,timeinfo%ptr)
        write(*,*) buffer(1:ret)

        rawtime = ftime()
        timeinfo = flocaltime(rawtime)
        ret = strftime(buffer,80_c_size_t,"Now it's %I:%M%p."//c_null_char,timeinfo%ptr)
        write(*,*) buffer(1:ret)
    end subroutine

    subroutine strptime_example
        type(ctime_t), target :: rawtime
        type(ctm_t) :: timeinfo
        integer(c_int), target :: itm(11)

        type(c_ptr) :: ret
        real(c_double) :: seconds

        write(*,*)  new_line('a')//"strptime_example"

        rawtime%ptr = time(rawtime%ptr)

        itm = 0
        timeinfo%ptr = c_loc(itm)

        ret = strptime("1992-08-21"//c_null_char,"%F"//c_null_char,timeinfo%ptr)
        print *, itm
        seconds = difftime(rawtime%ptr,mktime(timeinfo%ptr))
        write(*,*) seconds, " seconds since I was born"
        write(*,*) seconds/(24.*3600.), " days since I was born"
        write(*,*) seconds/(365.24*24.*3600.), " years since I was born"
    end subroutine


    subroutine strerror_example
        integer(c_int) :: i

        write(*,*) new_line('a')//"strerror_example"
        i = test_strerror()
        write(*,'(A)') "The following error occurred: "//fstrerror(get_errno())
        call perror("The following error occurred"//c_null_char)
    end subroutine
end module

program test_libc

    use libc
    use iso_c_binding

    implicit none

    real(c_double) :: a
    type(c_ptr) :: p_b
    real(c_double), pointer :: b(:) => null()
 
    !
    ! <time.h>
    !
    call clock_example
    call difftime_example
    ! call mktime_example ! Expects user input
    call time_example
    call asctime_example
    call gmtime_example
    call localtime_example
    call strftime_example
    call strptime_example

    !
    ! <errno.h>
    !
    call strerror_example
    stop

    p_b = cmalloc(c_sizeof(a)*5)
    call c_f_pointer(p_b,b,[5])

    b = 1.0
    print *, b
    !b = 0.0
    p_b = crealloc(p_b,c_sizeof(a)*10)

    call c_f_pointer(p_b,b,[10])
    print *, b
    b(6:) = 2

    print *, b
    print *, c_associated(p_b), c_associated(p_b,c_loc(b)), associated(b)
    print *, c_sizeof(p_b), sizeof(b)
    nullify(b)
    call cfree(p_b)
    p_b = c_null_ptr
    print *, c_associated(p_b), c_associated(p_b,c_loc(b)), associated(b)

    call c_f_pointer(p_b,b,[5])
    print *, c_associated(p_b), c_associated(p_b,c_loc(b)), associated(b)
end program