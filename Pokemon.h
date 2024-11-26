#include <iostream>
using namespace std;
// Ejercicio 2
// Modelaremos los tipos de datos P okemon, como un TipoDePokemon (agua, fuego o planta,
// sinónimo de string) y un porcentaje de energía (que inicia en 100); y Entrenador, como un
// nombre, una cantidad de pokémon y un array de pokémon. Así, la representación es la siguiente:

typedef string TipoDePokemon;

struct PokeSt;

typedef PokeSt* Pokemon;

// Dicho esto, implementar la siguiente interfaz de P okemon:
// 
Pokemon consPokemon(TipoDePokemon tipo);
// Dado un tipo devuelve un pokémon con 100 % de energía.
// 
TipoDePokemon tipoDePokemon(Pokemon p);
// Devuelve el tipo de un pokémon.
// 
int energia(Pokemon p);
// Devuelve el porcentaje de energía.
// 
void perderEnergia(int energia, Pokemon p);
// Le resta energía al pokémon.
// 
bool superaA(Pokemon p1, Pokemon p2);
// Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
// Una vez hecho eso, implementar la siguiente interfaz de Entrenador:
// 
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon);
// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
// un entrenador.
// 
string nombreDeEntrenador(Entrenador e);
// Devuelve el nombre del entrenador.
// 
int cantidadDePokemon(Entrenador e);
// Devuelve la cantidad de pokémon que posee el entrenador.
// 
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);
// Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
// 
Pokemon pokemonNro(int i, Entrenador e);
// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i − 1 pokémon.
// 
bool leGanaATodos(Entrenador e1, Entrenador e2);
// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
// posee al menos un pokémon que le gane.