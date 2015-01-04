#include"tgen.h"
#include"b2t.h"
#include"sort.h"

BN* CreateNode(CSTRING str_tag){
	BN* cn=(BN*)malloc(sizeof(BN));
	cn->parent=NULL;
	cn->lchild=NULL;
	cn->rchild=NULL;
	memcpy(cn->tag,str_tag,strlen(str_tag));
	cn->tag[(int) strlen(str_tag)]='\0';
	cn->BranchLength=1;

	int taglen=(int) strlen(cn->tag);
	cn->index=B2D(cn->tag,taglen);
	cn->TreeInfo=(STRING)malloc(sizeof(char)*(taglen+3));
	memcpy(cn->TreeInfo,cn->tag,taglen*sizeof(char));
	cn->TreeInfo[taglen]=':';
	cn->TreeInfo[taglen+1]='1';
	cn->TreeInfo[taglen+2]='\0';
	return cn;
}

void UpdateNode(BN* Node,int if_tag){
	Node->BranchLength++;
	STRING cbl=itoa(Node->BranchLength);
	int clen=(int) strlen(cbl);
	int len=(int) strlen(Node->TreeInfo);
	int i;
	for(i=len-1;i>0;i--) if(Node->TreeInfo[i-1]==':') break;
	STRING NTreeInfo=(STRING)malloc(sizeof(char)*(clen+i+1));
	memcpy(NTreeInfo,Node->TreeInfo,sizeof(char)*i);
	memcpy(NTreeInfo+i,cbl,sizeof(char)*clen);
	NTreeInfo[clen+i]='\0';
	free(cbl);
	free(Node->TreeInfo);
	Node->TreeInfo=NTreeInfo;
	if(if_tag) Node->tag[(int)strlen(Node->tag)-1]='\0';
}

BN* MergeNode(BN* LNode,BN* RNode){
	BN* cn=(BN*)malloc(sizeof(BN));
	cn->parent=NULL;
	cn->lchild=LNode;
	cn->rchild=RNode;
	cn->BranchLength=1;
	int sl=(int) strlen(LNode->tag);
	int rl=(int) strlen(RNode->tag);
	int L_len=(int) strlen(LNode->TreeInfo);
	int R_len=(int) strlen(RNode->TreeInfo);
	STRING cnl=itoa(cn->lchild->BranchLength);
	STRING cnr=itoa(cn->rchild->BranchLength);

	if(sl!=1){
		memcpy(cn->tag,LNode->tag,sizeof(char)*sl);
		cn->tag[sl-1]='\0';
		cn->index=B2D(cn->tag,sl-1);

		cn->TreeInfo=(STRING)malloc(sizeof(char)*(L_len+R_len+6));
		cn->TreeInfo[0]='(';
		memcpy(cn->TreeInfo+1,LNode->TreeInfo,L_len*sizeof(char));
		cn->TreeInfo[L_len+1]=',';
		memcpy(cn->TreeInfo+L_len+2,RNode->TreeInfo,R_len*sizeof(char));
		cn->TreeInfo[L_len+R_len+2]=')';
		cn->TreeInfo[L_len+R_len+3]=':';
		cn->TreeInfo[L_len+R_len+4]='1';
		cn->TreeInfo[L_len+R_len+5]='\0';
	}
	else{
		cn->tag[0]='r';
		cn->tag[1]='\0';
		cn->index=-1;

		cn->TreeInfo=(STRING)malloc(sizeof(char)*(L_len+R_len+5));
		cn->TreeInfo[0]='(';
		memcpy(cn->TreeInfo+1,LNode->TreeInfo,L_len*sizeof(char));
		cn->TreeInfo[L_len+1]=',';
		memcpy(cn->TreeInfo+L_len+2,RNode->TreeInfo,R_len*sizeof(char));
		cn->TreeInfo[L_len+R_len+2]=')';
		cn->TreeInfo[L_len+R_len+3]=';';
		cn->TreeInfo[L_len+R_len+4]='\0';
	}
	return cn;
}

BN* MakeTree(STRING* Leaf,int NLeaf){
	if(!NLeaf) return NULL;
	random_quicksort(Leaf,0,NLeaf-1);
	DEPTH=strlen(Leaf[0]);
	BS* bs = (BS*)malloc(sizeof(BS)*NLeaf);
	int i;
	for(i=0;i!=NLeaf;i++){
		bs[i].curr=CreateNode(Leaf[i]);
		if(i!=NLeaf-1)bs[i].next=&bs[i+1];
		else bs[i].next=NULL;
		if(DEPTH<(int) strlen(Leaf[i])) DEPTH=(int) strlen(Leaf[i]);
	}	
	IS* is=(IS*)malloc(sizeof(IS)*NLeaf);
	for(i=0;i<NLeaf;i++){
		int idx=DEPTH-strlen(Leaf[i]);
		is[i].id=B2D(Leaf[i],(int) strlen(Leaf[i]))<<idx;
		if(i!=NLeaf-1)is[i].next=&is[i+1];
		else is[i].next=NULL;
	}
	
	int stack_size=NLeaf-1;
	IS* iseek;
	BS* bseek;
	/*bseek=bs;
	iseek=is;
	while(bseek){
		printf("tag:%s  id:%d TreeInfo:%s\n",bseek->curr->tag,iseek->id,bseek->curr->TreeInfo);
		bseek=bseek->next;
		iseek=iseek->next;
	}*/
	int if_rshuffle_tail=1;
	while(stack_size>0){
		bseek=bs;
		iseek=is;
		int i1,i2;
		BN* nbn;
		int tail_branch=0;
		int if_rshuffle_tail=1;
		while(iseek->next){
			i1=(int) ((unsigned) iseek->id)>>1;
			i2=(int) ((unsigned) iseek->next->id)>>1;
			if(i1==i2){
				nbn=MergeNode(bseek->curr,bseek->next->curr);
				iseek->id=(int) ((unsigned) iseek->id)>>1;
				bseek->curr=nbn;
				bseek->next=bseek->next->next;
				iseek->next=iseek->next->next;
				if(bseek->next){
					bseek=bseek->next;
					iseek=iseek->next;
					stack_size--;
				}
				else{
					stack_size--;
					tail_branch=1;
					if_rshuffle_tail=0;
					break;
				}	
			}
			else{
				if((int) strlen(bseek->curr->tag)==DEPTH)UpdateNode(bseek->curr,1);
				else UpdateNode(bseek->curr,0);
				iseek->id=(int) ((unsigned) iseek->id)>>1;
				if(bseek->next){
					bseek=bseek->next;
					iseek=iseek->next;
				}
				else break;
			}
		}
		if(!tail_branch){
			if((int) strlen(bseek->curr->tag)==DEPTH) UpdateNode(bseek->curr,1);
			else UpdateNode(bseek->curr,0);
		}	
		if(if_rshuffle_tail){
			iseek->id=(int) ((unsigned) iseek->id)>>1;
		}
		else{
			if_rshuffle_tail=1;
		}
		/*printf("when stack_size is:%d, Depth is: %d, the tree info is:\n",stack_size,DEPTH);
		bseek=bs;
		iseek=is;
		while(bseek){
			printf("tag:%s  id:%d TreeInfo:%s\n",bseek->curr->tag,iseek->id,bseek->curr->TreeInfo);
			bseek=bseek->next;
			iseek=iseek->next;
		}*/
		DEPTH--;
		//exit(1);
	}
	free(is);
	BN* ret=bs->curr;
	free(bs);
	return ret;
}

void DeleteTree(BN* Tree){
	if(!Tree) return ;
	BN* delete=Tree;
	if(delete->lchild) DeleteTree(delete->lchild);
	if(delete->rchild) DeleteTree(delete->rchild);
	//printf("Delete now:  %s %s\n",delete->tag,delete->TreeInfo);
	if(delete->TreeInfo)free(delete->TreeInfo);
	free(delete);
}

int main(int argc,char** argv){
	STRING* s=argv+1;
	BN* test=MakeTree(s,argc-1);
	printf("%s\n",test->TreeInfo);
	DeleteTree(test);
	return 0;
}
