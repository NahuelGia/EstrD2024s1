struct NodeT {
int elem;
NodeT* left;
NodeT* right;
};

typedef NodeT* Tree; // INV.REP: El puntero no es NULL

Tree emptyT();
Tree nodeT(int elem, Tree left, Tree right);
bool isEmptyT(Tree t);
int rootT(Tree t);
Tree left(Tree t);
Tree right(Tree t);