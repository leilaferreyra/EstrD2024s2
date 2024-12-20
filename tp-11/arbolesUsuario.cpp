#include "arbolesBinarios.cpp";
// 1.
int sumarT(Tree t)
{
    int suma = 0;
    if (!isEmptyT(t))
    {
        suma += rootT(t) + sumarT(left(t)) + sumarT(right(t));
    }
    return suma;
}
// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
// 2.
int sizeT(Tree t)
{
    int tamaño = 0;
    if (!isEmptyT(t))
    {
        tamaño += 1 + sizeT(left(t)) + sizeT(right(t));
    }
    return tamaño;
}
// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
// en inglés).
// 3.
bool perteneceT(int e, Tree t)
{
    if (!isEmptyT(t))
    {
        return (rootT(t) == e) || perteneceT(e, left(t)) || perteneceT(e, right(t));
    }
    return false;
}
// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
// árbol.
// 4.
int aparicionesT(int e, Tree t)
{
    int suma = 0;
    if (!isEmptyT(t))
    {
        if (rootT(t) == e)
        {
            1 + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        }
        else
        {
            aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        }
    }
    return suma;
}
// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
// 5.

int heightT(Tree t)
{
    int altura = 0;
    if (!isEmptyT(t))
    {
        altura += 1 + std::max(heightT(left(t)), heightT(right(t)));
    }
    return altura;
}

// Dado un árbol devuelve su altura.
// 6.
ArrayList toList(Tree t)
{
    ArrayList l = newArrayListWith(sizeT(t));
    return toListPrim(t, l);
}
ArrayList toListPrim(Tree t, ArrayList l)
{
    if (!isEmptyT(t))
    {
        add(rootT(t), l);
        toList(left(t), l);
        toList(right(t), l);
    }
    return l;
}

// Dado un árbol devuelve una lista con todos sus elementos.
// 7.
ArrayList leaves(Tree t)
{
    ArrayList l = newArrayListWith(sizeT(t)); // Inicializamos el ArrayList con una capacidad adecuada
    return leavesP(t, l);
}

ArrayList leavesP(Tree t, ArrayList l)
{
    if (!isEmptyT(t))
    {
        if (isEmptyT(left(t)) && isEmptyT(right(t)))
        {
            add(rootT(t), l);
        }
        else
        {
            leavesP(left(t), l);
            leavesP(right(t), l);
        }
    }
    return l;
}

// Dado un árbol devuelve los elementos que se encuentran en sus hojas.
// 8.
ArrayList levelN(int n, Tree t)
{
    ArrayList l = newArrayList();
    return levelNP(n, t, l);
}
ArrayList levelNP(int n, Tree t, ArrayList l)
{
    if (!isEmptyT(t) && n > 0)
    {
        levelNP((n - 1), t, l);
        levelNP((n - 1), t, l);
    }
    else
    {
        if (!isEmptyT(t) && n == 0)
        {
            add(rootT(t), l);
        }
    }
}
// Dados un número n y un árbol devuelve una lista con los nodos de nivel n.