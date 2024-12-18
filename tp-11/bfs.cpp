#include <iostream>
#include "ArrayList.h"
#include "queueTree.h"
#include "arbolesBinarios.h"
using namespace std;
// Ejercicio 8
// Denir las funciones del punto anterior utilizando BFS (recorrido iterativo a lo ancho), a excepción
// de heightT, leaves y levelN. Para esto, utilizar una Queue de Tree.

// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int sumarT(Tree t)
{
    if (isEmptyT(t))
    {
        return 0;
    }
    Queue q = emptyQ();
    Enqueue(t, q);
    int suma = 0;
    while (!isEmptyQ(q))
    {
        Tree current = firstQ(q);
        Dequeue(q);
        suma += rootT(current);
        if (!isEmptyT(left(current)))
        {
            Enqueue(left(current), q);
        }
        if (!isEmptyT(right(current)))
        {
            Enqueue(right(current), q);
        }
    }
    DestroyQ(q);
    return suma;
}

// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
// en inglés).
int sizeT(Tree t)
{
    if (isEmptyT(t))
    {
        return 0;
    }
    Queue q = emptyQ();
    Enqueue(t, q);
    int suma = 0;
    while (!isEmptyQ(q))
    {
        Tree current = firstQ(q);
        Dequeue(q);
        suma++;
        if (!isEmptyT(left(current)))
        {
            Enqueue(left(current), q);
        }
        if (!isEmptyT(right(current)))
        {
            Enqueue(right(current), q);
        }
    }
    DestroyQ(q);
    return suma;
}

// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
// árbol.
bool perteneceT(int e, Tree t)
{
    Queue q = emptyQ();
    Tree current = t;

    if (!isEmptyT(t))
    {
    Enqueue(t, q);
    }
    while (!isEmptyQ(q) && rootT(current)!=e)
    {
        current = firstQ(q);
        Dequeue(q);
        if (!isEmptyT(left(current)))
        {
            Enqueue(left(current), q);
        }
        if (!isEmptyT(right(current)))
        {
            Enqueue(right(current), q);
        }
    }
    DestroyQ(q)
    return current!=nullptr && rootT(current) == e;
}

// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
int aparicionesT(int e, Tree t) {
    int suma = 0;
    if (!isEmptyT(t)) {
        Queue q = emptyQ();
        Enqueue(t, q);
        while (!isEmptyQ(q)) {
            Tree current = firstQ(q);
            Dequeue(q);
            if (rootT(current) == e) {
                suma++;
            }
            if (!isEmptyT(left(current))) {
                Enqueue(left(current), q);
            }
            if (!isEmptyT(right(current))) {
                Enqueue(right(current), q);
            }
        }
        DestroyQ(q); 
    }
    return suma;
}


// Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {
    ArrayList list = newArrayList();
    if (!isEmptyT(t)) {
        Queue q = emptyQ();
        Enqueue(t, q);
        while (!isEmptyQ(q)) {
            Tree current = firstQ(q);
            Dequeue(q);
            add(rootT(current), list);
            if (!isEmptyT(left(current))) {
                Enqueue(left(current), q);
            }
            if (!isEmptyT(right(current))) {
                Enqueue(right(current), q);
            }
        }
        DestroyQ(q);
    }
    return list;
}




