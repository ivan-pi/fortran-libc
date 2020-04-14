#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

typedef struct {
    void *tm;
} ctm_t;
//typedef struct tm *ctm_t;

typedef struct {
    void *time_t;
} ctime_t;
//typedef time_t *ctime_t;

double mydifftime(ctime_t t1, ctime_t t2){
/*    time_t end, beginning;

    end = *(time_t *) &t1;
    beginning = *(time_t *) &t2;*/
    return difftime(*(time_t *) &t1,*(time_t *) &t2);
}

typedef clock_t *cclock_t;

clock_t get_clocks_per_sec(){
    return CLOCKS_PER_SEC;
}

void set_tm(struct tm *timeptr, int hour, int min, int sec, int year, int mon, int mday){
    //struct tm *timeptr = (struct tm *)t;  
    timeptr->tm_hour = hour;
    timeptr->tm_min = min;
    timeptr->tm_sec = sec;
    timeptr->tm_year = year;
    timeptr->tm_mon = mon;
    timeptr->tm_mday = mday;
}

int get_rand_max(void){
    return RAND_MAX;
}

void set_errno(int n){
    errno = n;
}

int get_errno(){
    return errno;
}

int test_strerror(){
  FILE * pFile;
  pFile = fopen ("unexist.ent","r");
  if (pFile == NULL)
    printf ("Error opening file unexist.ent: %s\n",strerror(errno));
  return 0;
}