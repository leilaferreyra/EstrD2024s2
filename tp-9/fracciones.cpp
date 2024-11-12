#include <iostream>
#include "fracciones.h"
using namespace std;
// // Propósito: construye una fraccion
// // Precondición: el denominador no es cero
// 
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador =  numerador; f.denominador = denominador;
    return (f);
}

// // Propósito: devuelve el numerador
int numerador(Fraccion f){
    return (f.numerador);
}
// // Propósito: devuelve el denominador
// 
int denominador(Fraccion f){
    return (f.denominador)
}

// // Propósito: devuelve el resultado de hacer la división
// 
float division(Fraccion f){
    return (f.numerador / f.denominador )
}

// // Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// // (sin simplificar)
// 
Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion f;
    f.numerador = f1.numerador * f2.numerador;
    f.denominador = f1.denominador * f2.numerador;
    return (f);
}

// // Propósito: devuelve una fracción que resulta
// // de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    Fraccion f; 
    int div;
    div = mcd(f.numerador, f.denominador);
    f.numerador = f.numerador / div;
    f.denominador = f.denominador / div;
    return f ;

}

int mcd(int a, int b) {
     if (b == 0) {
         return a; } 
         else { 
            return mcd(b, a % b); } 
            }

// // Propósito: devuelve la fracción resultante de sumar las fracciones
// 
Fraccion sumF(Fraccion f1, Fraccion f2){
    int denominador_comun = (a.denominador * b.denominador) / mcd(a.denominador, b.denominador); 
    int numerador_a = a.numerador * (denominador_comun / a.denominador); 
    int numerador_b = b.numerador * (denominador_comun / b.denominador);
     Fraccion resultado; 
     resultado.numerador = numerador_a + numerador_b; 
     resultado.denominador = denominador_comun; 
     return simplificada (resultado)
}