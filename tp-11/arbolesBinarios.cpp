// 4. Árboles binarios
// Ejercicio 6
// Dada esta denición para árboles binarios
struct NodeT {
int elem;
NodeT* left;
NodeT* right;
};
typedef NodeT* Tree;
// denir la siguiente interfaz:
// 
Tree emptyT(){
    return nullptr;
}
// 
Tree nodeT(int elem, Tree left, Tree right){
    NodeT* t= new NodeT;
    t->elem= elem;
    t->left= left;
    t->right= right;
    return t;
}
// 
bool isEmptyT(Tree t){
    return t == nullptr;
}
// 
int rootT(Tree t){
    return t->elem;
}
// 
Tree left(Tree t){
    return t->left;
}
// 
Tree right(Tree t){
    return t->right;
}
