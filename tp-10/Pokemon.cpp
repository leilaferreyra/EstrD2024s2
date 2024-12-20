#include <iostream>
#include "Personas.h"
using namespace std;
// Ejercicio 2
// Modelaremos los tipos de datos P okemon, como un T ipoDeP okemon (agua, fuego o planta,
// sinónimo de string) y un porcentaje de energía (que inicia en 100); y Entrenador, como un
// nombre, una cantidad de pokémon y un array de pokémon. Así, la representación es la siguiente:
// Página 1 de 4
// Estructuras de datos - UNQ
typedef string TipoDePokemon;
struct PokeSt
{
    TipoDePokemon tipo;
    int vida;
} typedef PokeSt *Pokemon;
struct EntrenadorSt
{
    string nombre;
    Pokemon *pokemon;
    int cantPokemon;
} typdef EntrenadorSt *Entrenador;
// Dicho esto, implementar la siguiente interfaz de P okemon:
//
Pokemon consPokemon(TipoDePokemon tipo)
{
    PokeSt *p = new Pokemon;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}
// Dado un tipo devuelve un pokémon con 100 % de energía.
//
TipoDePokemon tipoDePokemon(Pokemon p)
{
    return p->tipo;
}
// Devuelve el tipo de un pokémon.
//
int energia(Pokemon p)
{
    return p->vida;
}
// Devuelve el porcentaje de energía.
//
void perderEnergia(int energia, Pokemon p)
{
    p->vida -= energia;
}
// Le resta energía al pokémon.
//
bool superaA(Pokemon p1, Pokemon p2)
{
    TipoDePokemon p1t = p1->tipo;
    TipoDePokemon p2t = p2->tipo;

    return p1t == "Agua" && p2t == "Fuego" ||
           p1t == "Fuego" && p2t == "Planta" ||
           p1t == "Planta" && p2t == "Agua";
}
// Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
// Una vez hecho eso, implementar la siguiente interfaz de Entrenador:
//
Entrenador consEntrenador(string nombre, int cantidad, Pokemon *pokemon)
{
    EntrenadorSt *e = new EntrenadorSt *;
    e->nombre = nombre;
    e->pokemon = pokemon;
    e->cantPokemon = cantidad;
    return e;
}
// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
// un entrenador.
//
string nombreDeEntrenador(Entrenador e)
{
    return e->nombre;
}
// Devuelve el nombre del entrenador.
//
int cantidadDePokemon(Entrenador e)
{
    return e->cantPokemon;
}
// Devuelve la cantidad de pokémon que posee el entrenador.
//
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e)
{
    int contador = 0;
    for (i = 0; e->cantPokemon > i; i++)
    {
        if (tipoDePokemon(e->pokemon[i]) == tipo)
        {
            contador++;
        }
    }
    return contador;
}
// Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
//
Pokemon pokemonNro(int i, Entrenador e)
{
    return e->pokemon[i + 1];
}
// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i − 1 pokémon.
//
bool leGanaATodos(Entrenador e1, Entrenador e2)
{

    Pokemon currentPk = e2->pokemon[0] for (i = 1; leGanaAlguno(currentPk, e1->pokemon); i++)
    {
        currentPk = e2->pokemon[i];
    }
    return leGanaAlguno(currentPk, e1->pokemon);
}

bool pkLeGanaATodos(Pokemon p; Entrenador e)
{
    int vencidos = 0;
    for (i = 0; (i < e->cantPokemon) && superaA(p, e->pokemon[i]); i++)
    {
        vencidos++
    }
    return vencidos == e->cantPokemon;
}

bool leGanaATodos(Entrenador e1, Entrenador e2)
{
    int vencedores = 0;
    for (int i = 0; i < e1->cantPokemon; i++)
    {
        if (pkLeGanaATodos(e1->pokemon[i], e2))
        {
            vencedores++
        }
    }
    return vencedores > 0;
}

// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
// posee al menos un pokémon que le gane.
