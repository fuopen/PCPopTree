#include"sort.h"

void rswap(STRING* a, STRING* b){
	STRING tmp = *a;
	*a = *b;
	*b = tmp;
}

int sample(int begin,int end){
	if(begin>end){
		perror("error when the begin greater than end!\n");
		exit(2);
	}
	if(begin==end) return begin;
	int len = end-begin+1;
	int rand_index = rand() % len + begin;
	return rand_index;
}

int partition(STRING* strs,int begin,int end){
	STRING x = strs[end];
	int i = begin-1,j;
	for(j=begin;j<end;j++){
		//if(strs[j]<=x){
		if(strcmp(strs[j],x)<=0){
			i++;
			rswap(&strs[i],&strs[j]);
		}
	}
	rswap(&strs[i+1],&strs[end]);
	return i+1;
}

int random_partition(STRING* strs,int begin,int end){
	int s = sample(begin,end);
	rswap(&strs[end],&strs[s]);
	return partition(strs,begin,end);
}

STRING random_select(STRING* strs,int begin,int end,int i){
	if(begin==end){
		return strs[begin];
	}
	int q = random_partition(strs,begin,end);
	int k = q-begin+1;

	if(i==k){
		return strs[q];
	}
	else if(i<k){
		return random_select(strs,begin,q-1,i);
	}
	else return random_select(strs,q+1,end,i-k);
}

void random_quicksort(STRING* strs,int begin,int end){
	if(begin>=end){
		return;
	}
	int q = random_partition(strs,begin,end);
	random_quicksort(strs,begin,q-1);
	random_quicksort(strs,q+1,end);
}
/*
int main(){
	//STRING s[7]={"0001","0000","11","010","011","001","10"};
	STRING s[38]={"000","0010000","001000100","0010001010","00100010110","001000101110","001000101111","00100011","001001","00101","0011","01000", "01001","0101","01100","011010","011011","011100","0111010","0111011",   "01111","1000","100100","1001010","1001011","10011","10100","1010100",   "1010101","101011","1011","110","11100","11101","111100","111101",      "111110","111111"};
	int i;
	for(i=0;i<38;i++) printf("%d: %s\n",i,s[i]);
	random_quicksort(s,0,37);
	printf("************\n");
	for(i=0;i<38;i++) printf("%d: %s\n",i,s[i]);
	return 0;
}*/
