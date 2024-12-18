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
// 
Set emptyS(){
    SetSt* s = new SetSt;
    s-> cantidad = 0;
    s->primero = nullptr;
    return s;
}
// Crea un conjunto vacío.
// 
bool isEmptyS(Set s){
    return s->cantidad == 0;
}
// Indica si el conjunto está vacío.
// 
bool belongsS(int x, Set s){
    NodoS* current = s->primero;
    while (current!=nullptr &&current->elem!=x){
        current= current->siguiente;
    }
    return current!=nullptr && current->elem == x;
}

bool perteneceAux (int x, Set s){
    NodoS* current = s->primero;
    while (current!=nullptr &&current->elem!=x){
        current= current->siguiente;
    }
    return current!=nullptr && current->elem == x;
}
// Indica si el elemento pertenece al conjunto.
// 
void AddS(int x, Set s){
    if (!perteneceAux(x,s)){
    NodoS* nodo = new NodoS;
    nodo-> elem = x;
    nodo-> siguiente = s->primero;
    s->cantidad++;
    s->primero=nodo;
    }
    
}
// Agrega un elemento al conjunto.
// 
void RemoveS(int x, Set s){
    NodoS* anterior;
    NodoS* current= s->primero;
    while (current==nullptr && current->elem!=x){
        anterior= current;
        current= current->siguiente;
    }
    if (current!=nullptr){
        if (s->primero->elem == x){
            s->primero= s->primero->siguiente;
        } else {
            anterior->siguiente = current->siguiente;
        }
        delete(current);
        s->cantidad--;
    }
}
// Quita un elemento dado.
// 
int sizeS(Set s){
    return s->cantidad;
}
// Devuelve la cantidad de elementos.
// 
LinkedList setToList(Set s){
    LinkedListSt* l= new LinkedListSt;
    l->primero=s->primero;
    l->cantidad=s->cantidad;
    return l;
}
// Devuelve una lista con los lementos del conjunto.
// 
void DestroyS(Set s){
    NodoS* current=s->primero;
    while (current!=nullptr){
        s->primero= s->primero->siguiente;
        delete(current);
        current= s->primero;
    }
    delete(s);
}
// Libera la memoria ocupada por el conjunto.
