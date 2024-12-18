// 3. Queue
// Ejercicio 5
// Dada la siguiente representación de colas:

struct NodoQ
{
    int elem;         // valor del nodo
    NodoQ *siguiente; // puntero al siguiente nodo
};
struct QueueSt
{
    int cantidad;   // cantidad de elementos
    NodoQ *primero; // puntero al primer nodo
    NodoQ *ultimo;  // puntero al ultimo nodo
};
typedef QueueSt *Queue;
// Denir la siguiente interfaz de este tipo de colas, respetando el costo de las operaciones:
//
Queue emptyQ()
{
    QueueSt *q = new QueueSt;
    q->cantidad = 0;
    q->primero = nullptr;
    q->ultimo = nullptr;
}
// Crea una cola vacía.
// Costo: O(1).
//
bool isEmptyQ(Queue q)
{
    return q->cantidad == 0;
}
// Indica si la cola está vacía.
// Costo: O(1).
//
int firstQ(Queue q)
{
    return q->primero->elem;
}
// Devuelve el primer elemento.
// Costo: O(1).
//
void Enqueue(int x, Queue q)
{
    NodoQ *nodo = new NodoQ;
    nodo->elem = x;
    nodo->siguiente = nullptr;
    if (q->cantidad == 0)
    {
        q->primero = nodo;
    }
    else
    {
        q->ultimo->siguiente = nodo;
    }
    q->ultimo = nodo;
    q->cantidad++;
}
// Agrega un elemento al nal de la cola.
// Costo: O(1).
//
void Dequeue(Queue q)
{
    NodoQ *primero = q->primero;
    if (q->cantidad > 0)
    {
        NodoQ *nuevoPrimero = primero->siguiente;
        q->primero = nuevoPrimero;
        delete (primero);
        q->cantidad--;
    }
}
// Quita el primer elemento de la cola.
// Costo: O(1).
//
int lengthQ(Queue q)
{
    return q->cantidad;
}
// Devuelve la cantidad de elementos de la cola.
// Costo: O(1).
//
void MergeQ(Queue q1, Queue q2)
{
    if (q1->cantidad > 0)
    {
        q1->ultimo->siguiente = q2->primero;
    }
    else
    {
        q1->primero = q2->primero;
    }
    q1->ultimo = q2->ultimo;
    q1->cantidad += q2->cantidad;
    delete (q2);
}
// Anexa q2 al nal de q1, liberando la memoria inservible de q2 en el proceso.
// Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
// Costo: O(1).
//
void DestroyQ(Queue q)
{
    for (int i = 0; i < q->cantidad; i++)
    {
        NodoQ *anterior = q->primero;
        q->primero = anterior->siguiente;
        delete (anterior);
    }
    delete (q);
}
// Libera la memoria ocupada por la cola.
// Costo: O(n).