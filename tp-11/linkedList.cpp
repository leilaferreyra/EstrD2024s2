struct NodoL {
int elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
}
struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
int cantidad; // cantidad de elementos
NodoL* primero; // puntero al primer nodo
}
typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL
struct IteratorSt {
NodoL* current;
}
// typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL
// Denir la siguiente interfaz de este tipo de listas, indicando el costo obtenido (intentar que sea
// lo más eciente posible):
// 
LinkedList nil(){
    LinkedListSt* l = new LinkedListSt;
    l->cantidad=0;
    l->primero=nullptr;
    return l;
}
// Crea una lista vacía.
// 
bool isEmpty(LinkedList xs){
    return xs->cantidad==0;
}
// Indica si la lista está vacía.
// 
int head(LinkedList xs){
    return xs->primero->elem;
}
// Devuelve el primer elemento.
// 
void Cons(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente=xs->primero;
    xs->primero=nodo;
    xs->cantidad++;
}
// Agrega un elemento al principio de la lista.
// 
void Tail(LinkedList xs){
    if(xs->cantidad>0){
        NodoL* eliminado = xs->primero;
        xs->primero=xs->primero->siguiente;
        delete(eliminado);
        xs->cantidad--;
    }
}
// Quita el primer elemento.
// 
int length(LinkedList xs){
    return xs->cantidad;
}
// Devuelve la cantidad de elementos.
// 
void Snoc(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem=x;
    nodo->siguiente=nullptr;
    
    if (xs->cantidad>0){
    NodoL* current= xs->primero;
    while (current->siguiente!=nullptr){
        current=current->siguiente;
    }
    current->siguiente=nodo;
    } else {
        xs->primero=nodo;
    }
    xs->cantidad++;

}
// Agrega un elemento al nal de la lista.
// 
ListIterator getIterator(LinkedList xs){
    ListIterator* i = new ListIerator;
    i->current=xs->primero;
    return i;

}
// Apunta el recorrido al primer elemento.
// 
int current(ListIterator ixs){
    return ixs->current->elem;
}
// Devuelve el elemento actual en el recorrido.
// 
void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem=x;
}
// Reemplaza el elemento actual por otro elemento.
// 
void Next(ListIterator ixs){
    ixs->current=ixs->current->siguiente;
}
// Pasa al siguiente elemento.
// 
bool atEnd(ListIterator ixs){
    return ixs->current ==nullptr;
}
// Indica si el recorrido ha terminado.
// 
void DisposeIterator(ListIterator ixs){
    delete(ixs);
}
// Libera la memoria ocupada por el iterador.
// 
void DestroyL(LinkedList xs){
    NodoL* current= xs->primero;
    while (current->siguiente!=nullptr){
        xs->primero=xs->primero->siguiente;
        delete(current);
        current=xs->primero;
    }
    delete(xs);
}
// Libera la memoria ocupada por la lista.

// 1. 
int sumatoria(LinkedList xs){
    IteratorSt* i = getIterator(xs);
    int suma=0; 
    while (!atTheEnd(i)){
        suma+=current(i);
        Next(i);
    }
    DisposeIterator(i);
    return suma;

}
// Devuelve la suma de todos los elementos.
// 2. 
void Sucesores(LinkedList xs){
    IteratorSt* i = getIterador(xs);
    while(!atTheEnd(i)){
        Set(current(i)++, i);
        Next(i)
    }
    DisposeIterator(i);
}
// Incrementa en uno todos los elementos.
// 3. 
bool pertenece(int x, LinkedList xs){
    IteratorSt* i = getIterator(xs);
    int actual = current(xs);
    while (!atTheEnd(i)&&actual!=x){
        Next(i);
        actual = current(xs);
    }
    DisposeIterator(i);
    return actual!=nullptr && actual==x;
}
// Indica si el elemento pertenece a la lista.
// 4. 
int apariciones(int x, LinkedList xs){
    IteratorSt* i=getIterator(xs);
    int cantidad = 0;
    while(!atTheEnd(i)){
        if (current(i)==x){
            cantidad++;
        }
        Next(i);
    }
    DisposeIterator(i);
    return cantidad;
}
// Indica la cantidad de elementos iguales a x.
// 5. 
int minimo(LinkedList xs){
    //Precondicion: La lista no esta vacia.
    IteratorSt* i=getIterator(xs);
    int minimo = current(i);
    while(!atTheEnd(i)){
        if (minimo>current(i)){
            minimo=current(i)
        }
        Next(i);
    }
    DisposeIterator(i);
    return minimo;
}
// Devuelve el elemento más chico de la lista.
// 6. 
LinkedList copy(LinkedList xs){
    IteratorSt* i = getIterador(xs);
    LinkedListSt* l= new LinkedListSt;
    while (!atTheEnd(i)){
        Snoc(current(i),l);
        Next(i)

    }
    DisposeIterator(i);
    return l;
}
// Dada una lista genera otra con los mismos elementos, en el mismo orden.
// Nota: notar que el costo mejoraría si Snoc fuese O(1), ¾cómo podría serlo?
void Append(LinkedList xs, LinkedList ys){
    IteratorSt* i = getIterator(ys);
    while (!atTheEnd(ys)){
        Snoc(current(i),xs);
        Next(i);
    }
    DisposeIterator(i);
    DistroyL(ys);
}
// Agrega todos los elementos de la segunda lista al nal de los de la primera.
// La segunda lista se destruye.
// Nota: notar que el costo mejoraría si Snoc fuese O(1), ¾cómo podría serlo?
