#ifndef _SORT_H_
#define _SORT_H_


#include<stdio.h>
#include<stdlib.h>
#include<time.h>
#include"macro.h"

static void rswap(STRING* a,STRING* b);
static int sample(int begin,int end);
static int partition(STRING* strs,int begin,int end);
static int random_partition(STRING* strs,int begin,int end);
static STRING random_select(STRING* strs,int begin,int end,int i);

void random_quicksort(STRING* strs,int begin,int end);

#endif
