#include <iostream>
#include "Par.h"
using namespace std;

Par consPar(int x, int y){
    Par p;
    p.x = x; p.y = y;
    return (p);
}

int fst(Par p){
    return (p.x);
}

int snd(Par p){
    return (p.y);
}

int maxDelPar(Par p){
    if (p.x >= p.y)
            {return (p.x);}
        else  {return (p.y);}
    }

Par swap(Par p) {
// Propósito: devuelve un par con las componentes intercambiadas
    Par par;
    par = consPar (p.y , p.x);
    return (par);
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m){
    Par p; 
    int division;
    int resto;
    division = n / m;
    resto = n % m;
    p = consPar (division, resto);
    return (p); 
}

void showPar(Par p) {
    cout << "(" << p.x << "," << p.y << ")" << endl;
}






