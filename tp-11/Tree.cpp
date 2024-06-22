#include "Tree.h"


Tree emptyT(){ // Un árbol vacío es simplemente NULL
    return nullptr;
}

Tree nodeT(int elem, Tree left, Tree right){
    Tree t = new NodeT;
    t->elem = elem;
    t->left = left;
    t->right = right;

    return t;
}

bool isEmptyT(Tree t){
    return t == nullptr;
}

int rootT(Tree t){
    return t->elem; 
}

Tree left(Tree t){
    return t->left;
}

Tree right(Tree t){
    return t->right;
}


