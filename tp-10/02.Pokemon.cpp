#include <iostream>
#include "Pokemon.h"
using namespace std;

struct EntrenadorSt {
string nombre;
Pokemon* pokemon;
int cantPokemon;
}

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    EntrenadorSt* e = new EntrenadorSt;
    e-> nombre = nombre; e-> cantPokemon = cantidad; e-> pokemon = pokemon;
    return e;
}
// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
// un entrenador.
// 
string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}
// Devuelve el nombre del entrenador.
// 
int cantidadDePokemon(Entrenador e){
    return e-> cantPokemon;
}
// Devuelve la cantidad de pokémon que posee el entrenador.
// 
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
}

int cantidadDePokemonDe (TipoDePokemon tipo, Pokemon* p ){
    int i = 0;
    int cant = 0;

    for 
    
}

int mult(int n, int m){
    if (n==0 || m == 0){
        return 0;
    } else {
        return m + mult(--n , m);
    }
}
// Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
// 
Pokemon pokemonNro(int i, Entrenador e);
// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i − 1 pokémon.
// 
bool leGanaATodos(Entrenador e1, Entrenador e2);
// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
// posee al menos un pokémon que le gane.