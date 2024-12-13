#include <iostream>
#include "ArrayList.h"
using namespace std;

// Denir las siguientes funciones utilizando la interfaz de ArrayList:
// 1. 
int sumatoria(ArrayList xs){
    int suma= 0;
    for (int i = 0; i < lengthAL(xs); i++){
        suma+= get(i,xs);
    }
    return suma;
}
// Devuelve la suma de todos los elementos.

// 2. 
void sucesores(ArrayList xs){
    for (int i=0; i < lengthAL(xs); i++){
        set(i, get(i,xs) + 1, xs);
    }
}
// Incrementa en uno todos los elementos.
// 3. 
//Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs) {
    int i = 0;
    while (i < lengthAL(xs) && get(i, xs) != x) {
        i++;
    }
    // Verificamos si el índice es válido antes de comparar
    return i < lengthAL(xs);
}

// Indica si el elemento pertenece a la lista.
// 4. 
int apariciones(int x, ArrayList xs){
    int cantidad = 0;
    for (int i=0; i<lengthAL(xs); i++){
        if (get(i,xs) == x) {
            cantidad++;
        }
    }
    return cantidad;
}
// Indica la cantidad de elementos iguales a x.
// 5. 
ArrayList append(ArrayList xs, ArrayList ys){
    ArrayList nuevaLista = newArrayListWith (lengthAL(xs) + lengthAL(ys));
    for (int i=0; i<lengthAL(xs); i++){
        add(get(i,xs), nuevaLista);
    }
    for (int i=0; i<lengthAL(ys); i++){
        add(get(i,ys), nuevaLista);
    }
    return nuevaLista;
}
// Crea una nueva lista a partir de la primera y la segunda (en ese orden).
// Página 3 de 4
// Estructuras de datos - UNQ
// 6. 
int minimo(ArrayList xs) {
    // Precondición: ArrayList tiene al menos un elemento.
    int largo = lengthAL(xs);
    int min = get(0, xs); // Inicializamos min con el primer elemento del ArrayList
    int actual;
    for (int i = 1; i < largo; i++) { // Iniciamos el bucle en 1
        actual = get(i, xs); // Obtenemos el valor actual del ArrayList
        if (min > actual) {
            min = actual; // Actualizamos min si encontramos un valor menor
        }
    }
    return min;
}

// Devuelve el elemento más chico de la lista.
