#include <iostream>
// Ejercicio 1
// Gracar la memoria resultante al ejecutar los siguientes programas, con foco en los cambios que
// se van realizando.
// 1.
int main()
{
    int x = 0;
    int y = 2;
    x = x + y;
}
//////////////////
//   (0)   (2)  //
//     x    y   //
//              //
//              //
//////////////////
// 2.
int main()
{
    int x = 0;
    int y = 0;
    while (y < 5)
    {
        x += y;
        y++;
    }
}

// 3.
int main()
{
    int y = 0;
    bool b = true;
    while (b)
    {
        y++;
        b = !b;
    }
}
//2 PROPOSITO

//1.
 //Proposito: Imprime todos los caracteres en el rango de c1 y c2, separandolos por una coma. 
 // Precondición: c1 < c2
    void
    printFromTo(char c1, char c2)
{
    for (int i = 0; c1 + i <= c2; i++)
    {
        cout << c1 + i << ", ";
    }
    cout << endl;
}
//2. 
//Proposito: Calcula el factorial del numero n. 
// Precondición: n >= 0
int fc(int n) {
int x = 1;
while(n > 0) {
x = x * n;
n--;
}
return x;
}
//3.
//Proposito: Suma todos los numeros entre n y m. 
 // Precondición: n <= m
int ft(int n, int m)
{
    if (n == m)
    {
        return n;
    }
    return n + ft(n + 1, m);
}

// Ejercicio 3
// Dada la estructura de pares representada como struct en C++, denir las siguientes funciones
// sobre pares. Recordar probar las implementaciones en un procedimiento main.
struct Par {
    int x;
    int y;
};

// Propósito: construye un par
Par consPar(int x, int y) {
    Par p;
    p.x = x;
    p.y = y;
    return p;
}

int main() {
    Par p = consPar(3, 4);
    std::cout<< "Par: (" << p.x << ", " << p.y << ")"<< std::endl;
    return 0;
}

//// Propósito: construye un par
// Par consPar(int x, int y);
// // Propósito: devuelve la primera componente
// int fst(Par p);
// // Propósito: devuelve la segunda componente
// int snd(Par p);
// // Propósito: devuelve la mayor componente
// int maxDelPar(Par p);
// // Propósito: devuelve un par con las componentes intercambiadas
// Par swap(Par p);
// // Propósito: devuelve un par donde la primer componente
// // es la división y la segunda el resto entre ambos números
// Par divisionYResto(int n, int m);
