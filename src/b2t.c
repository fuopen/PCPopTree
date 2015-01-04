#include "b2t.h"

int ndec(int dec){
	int f=1;
	while(dec/10){
		f++;
		dec/=10;
	}
	return f;
}

int intpow(const int a,const int b){
	int power=1;
	int i;
	for(i=0;i<b;i++) power*=a;
	return power;
}

STRING itoa(int dec){
	STRING str=(STRING)malloc(sizeof(char)*INTBUFFER);
	int len=ndec(dec);
	int i;
	for(i=len;i>0;i--){
		char s = dec/intpow(10,i-1)+48;
		str[len-i]=s;
		dec = dec%intpow(10,i-1);
	}
	str[len]='\0';
	return str;
}

void D2B(const int dec,STRING binary,int len){
	int p=dec,q;
	int index[BINBUFFER]={0};
	int count=BINBUFFER-1;
	while(p){
		q=(p>>1)<<1;
		index[count]=(p==q)?0:1;
		p=p>>1;
		count--;
	}
	q=0;
	while(q!=BINBUFFER){
		if(index[q]==1) break;
		q++;
	}
	len=BINBUFFER-q+1;
	int s=0;
	while(s!=len-1) binary[s++]=index[s+q]?'1':'0';
	binary[len-1]='\0';
}

int B2D(CSTRING binstr,int len){
	if(!len) return 0;
	int dec=0;
	int i,k;
	for(i=0;i!=len;i++){
		k=binstr[i]=='1'?1:0;
		dec+=k<<(len-1-i);
	}
	return dec;
}
/*
int main(int argc,char** argv){
	BIN_STRING test="001100011011";
	test[BINBUFFER-1]='\0';
	int b2d=B2D(test,(int)strlen(test));
	printf("b2d/d: %d /x: %x \n",b2d,b2d);
	BIN_STRING TEST;
	D2B(b2d,TEST,(int)strlen(test));
	printf("TEST: %s \n",TEST);
	int z=atoi(argv[1]);
	printf("s is: %s\n",itoa(z));
	return 0;
}*/
