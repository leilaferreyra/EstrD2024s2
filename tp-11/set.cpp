// 2. Set
// Ejercicio 4
// Dada la siguiente representación de conjuntos:
struct NodoS {
int elem; // valor del nodo
NodoS* siguiente; // puntero al siguiente nodo
}
struct SetSt {
int cantidad; // cantidad de elementos diferentes
NodoS* primero; // puntero al primer nodo
}
typedef SetSt* Set;
// Denir la siguiente interfaz de este tipo de conjuntos, indicando el costo obtenido (intentar que
// sea lo más eciente posible):
// Set emptyS()
// Crea un conjunto vacío.
// bool isEmptyS(Set s)
// Indica si el conjunto está vacío.
// bool belongsS(int x, Set s)
// Indica si el elemento pertenece al conjunto.
// void AddS(int x, Set s)
// Agrega un elemento al conjunto.
// 
void RemoveS(int x, Set s){ //Es lineal en la cantidad de elementos del set. //Memoria: Constante porque no hay ninguna llamada a funcion. 
    NodoS* actual = s-> primero;
    NodoS* anterior;
    while (actual!=nullptr && actual-> elem!=x){
        anterior = actual; 
        actual = actual -> siguiente;
    }
    if (actual!=nullptr){ //No usar elem con null
      anterior -> siguiente = actual-> siguiente;
      delete(actual);
      s-> cantidad--;
    }
}
// Quita un elemento dado.
// int sizeS(Set s)
// Devuelve la cantidad de elementos.
// LinkedList setToList(Set s)
// Devuelve una lista con los lementos del conjunto.
// void DestroyS(Set s)
// Libera la memoria ocupada por el conjunto.
