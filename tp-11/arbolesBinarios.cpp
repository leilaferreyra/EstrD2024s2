// 4. Árboles binarios
// Ejercicio 6
// Dada esta denición para árboles binarios
struct NodeT {
int elem;
NodeT* left;
NodeT* right;
}
typedef NodeT* Tree;
// denir la siguiente interfaz:
Tree emptyT()
Tree nodeT(int elem, Tree left, Tree right)
bool isEmptyT(Tree t)
int rootT(Tree t)
Tree left(Tree t)
Tree right(Tree t)
// Ejercicio 7
// Denir las siguientes funciones utilizando la interfaz de árbol y recursión:
// 1. int sumarT(Tree t)
// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
// 2. int sizeT(Tree t)
// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
// en inglés).
// 3. bool perteneceT(int e, Tree t)
// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
// árbol.
// 4. int aparicionesT(int e, Tree t)
// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
// 5. int heightT(Tree t)
// Dado un árbol devuelve su altura.
// 6. ArrayList toList(Tree t)
// Dado un árbol devuelve una lista con todos sus elementos.
// 7. ArrayList leaves(Tree t)
// Dado un árbol devuelve los elementos que se encuentran en sus hojas.
// 8. 
ArrayList levelN(int n, Tree t){ // 
    Array

}

//add Root (O(1) amortizado)
void levelAux // O(t) Es lineal en la profundidad del arbol. Memoria = Tantos stacks como la rama mas larga.
// Dados un número n y un árbol devuelve una lista con los nodos de nivel n.