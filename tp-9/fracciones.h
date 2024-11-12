#include <iostream>
using namespace std;
// Ejercicio 5
// Dada la estructura de fracciones representada como struct en C++, denir las siguientes funciones
// sobre fracciones. Recordar probar las implementaciones en un procedimiento main.
struct Fraccion {
int numerador;
int denominador;
};
// // Propósito: construye una fraccion
// // Precondición: el denominador no es cero
// 
Fraccion consFraccion(int numerador, int denominador);
// // Propósito: devuelve el numerador
// 
int numerador(Fraccion f);
// // Propósito: devuelve el denominador
// 
int denominador(Fraccion f);
// // Propósito: devuelve el resultado de hacer la división
// 
float division(Fraccion f);
// // Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// // (sin simplificar)
// 
Fraccion multF(Fraccion f1, Fraccion f2);
// // Propósito: devuelve una fracción que resulta
// // de simplificar la dada por parámetro
// Página 3 de 4
// Estructuras de datos - UNQ
// 
Fraccion simplificada(Fraccion p);
// // Propósito: devuelve la fracción resultante de sumar las fracciones
// 
Fraccion sumF(Fraccion f1, Fraccion f2);