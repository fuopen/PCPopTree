#ifndef _TGEN_H_
#define _TGEN_H_

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include"macro.h"

#define MAX_TREE_DEPTH 8

static int DEPTH=0;

typedef struct BinNode{
	struct BinNode* parent;
	struct BinNode* lchild;
	struct BinNode* rchild;
	BIN_STRING tag;
	int BranchLength;
	int index;
	STRING TreeInfo;
}BN;
typedef struct BinStack{
	struct BinStack* next;
	BN* curr;
}BS;
typedef struct IntStack{
	struct IntStack* next;
	int id;
}IS;

static BN* CreateNode(CSTRING str_tag);
static void DeleteNode(BN* Node);
static void UpdateNode(BN* Node,int if_tag);
static BN* MergeNode(BN* LNode,BN* RNode);
BN* MakeTree(STRING* Leaf,int NLeaf);
void DeleteTree(BN* Tree);
#endif
