
#include <iostream>
#include "Set.h"
using namespace std;

struct NodoS {
int elem; // valor del nodo
NodoS* siguiente; // puntero al siguiente nodo
};

struct SetSt {
//INV.REP: 
//*el set no admite elementos repetidos.
//*cantidad indica la cantidad de elementos del conjunto.
int cantidad; // cantidad de elementos diferentes
NodoS* primero; // puntero al primer nodo
};

// Set emptyS()
// Crea un conjunto vacío.
Set emptyS() {
    SetSt* s = new SetSt;
    s->cantidad=0;
    s->primero=NULL;
    return s;
}

// Indica si el conjunto está vacío.
bool isEmptyS(Set s) {
    return s->cantidad==0;
}

bool perteneceAux(int x, Set s) {
    if(s->cantidad!=0) {
        NodoS* actual = s->primero;
        while((actual->siguiente!=NULL) && ((actual->elem!=x))) {
            actual = actual->siguiente;
        }
        return (actual->elem == x);
        }
    return false;
}



// Indica si el elemento pertenece al conjunto.
bool belongsS(int x, Set s) {
    return perteneceAux(x,s);
}


void AddS(int x, Set s) {
    //reviso si ya está o no
    if (!perteneceAux(x,s)) {
        s->cantidad++;
        //armo el nodo nuevo
        NodoS* nuevo = new NodoS;
        nuevo->elem = x;
        nuevo->siguiente=s->primero;
        //lo acomodo en el Set
        s->primero=nuevo;
    }    
}


// Quita un elemento dado.
void RemoveS(int x, Set s) {
    NodoS* actual = s->primero;
    NodoS* anterior;
    while(actual!=NULL && actual->elem!=x) {
        anterior = actual;
        actual = actual->siguiente;
    }
    if (actual!=NULL ) {
        if(s->primero->elem == x) {
            s->primero = s->primero->siguiente;
        }
        else { 
        anterior->siguiente = actual->siguiente;
        }
        delete(actual);
        s->cantidad--;
    }
}


// Devuelve la cantidad de elementos.
int sizeS(Set s) {
    return s->cantidad;
}

// Devuelve una lista con los lementos del conjunto.
LinkedList setToList(Set s) {
    NodoS* actual = s->primero;
    LinkedList lista = nil();
    while(actual!=nullptr) {  
        Cons(actual->elem, lista);
        actual = actual->siguiente;
    }
    return lista;
}

// Libera la memoria ocupada por el conjunto.
void DestroyS(Set s) {
    NodoS* temp = s->primero;
    while(temp!=nullptr) {
        s->primero = s->primero->siguiente;
        delete temp;
        temp = s->primero;
    }
    delete s;
}
