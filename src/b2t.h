#ifndef _B2T_H_
#define _B2T_H_

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include"macro.h"

#define INTBUFFER 12

static int ndec(int dec);
static int intpow(const int a,const int b);

STRING itoa(int dec);
void D2B(const int dec,STRING binstr,int len);
int B2D(CSTRING binstr,int len);
#endif
