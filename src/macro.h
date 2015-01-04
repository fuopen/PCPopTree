#ifndef _MACRO_H_
#define _MACRO_H_

#define BUFFER 1024
#define BINBUFFER 32
#define PSTR(S) #S
#define STRLEN(S) (int)sizeof(S)
#define MIN(A,B) (A<B)?A:B
#define MAX(A,B) (A>B)?A:B

typedef char* STRING;
typedef const char* CSTRING;
typedef char BIN_STRING[BINBUFFER];
#endif
