#include <iostream>
#include "Pokemon.h"
using namespace std;

struct PokeSt {
TipoDePokemon tipo;
int vida;
}

Pokemon consPokemon(TipoDePokemon tipo){
    PokeSt* p= new PokeSt;
    p-> tipo = tipo;
    p-> vida = 100;
    return p;
    
}
// Dado un tipo devuelve un pokémon con 100 % de energía.
// 
TipoDePokemon tipoDePokemon(Pokemon p){
    return p->tipo;
}
// Devuelve el tipo de un pokémon.
// 
int energia(Pokemon p){
    return p-> vida;
}
// Devuelve el porcentaje de energía.
// 
void perderEnergia(int energia, Pokemon p){
    p-> vida--;
}
// Le resta energía al pokémon.
// 
bool superaA(Pokemon p1, Pokemon p2){
    return superaATipo (p1-> tipo, p2->tipo);

}

bool superATipo (TipoDePokemon t1, TipoDePokemon t2){
    return (t1 == "Agua" && t2 == "Fuego") || (t1 == "Fuego" && t2 == "Planta") || (t1 == "Planta" && t2 == "Agua") ;
}
// Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
// Una vez hecho eso, implementar la siguiente interfaz de Entrenador:
// 
