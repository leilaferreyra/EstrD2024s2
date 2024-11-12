// Ejercicio 4
// Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
// la menor cantidad posible de variables. Recordar denir subtareas en caso de que sea estrictamente
// necesario.
// 1. 
void printN(int n, string s){
// Propósito: imprime n veces un string s.
    string text = s;
     while (n > 0){
        cout<<s<<endl;
        n--;
     }
}
// 2. 
void cuentaRegresiva(int n){
    while (n>=0){
        cout<< n << endls;
        n--;
    }
}
// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
// 3. 
void desdeCeroHastaN(int n){
    int m
    m = 0
    while (m <= n){
        cout<< m << endls;
        m++;
    }
}
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
// 4. 
int mult(int n, int m){
    if (n==0 || m == 0){
        return 0;
    } else {
        return m + mult(--n , m);
    }
}
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
// 5. 
void primerosN(int n, string s){
    while (n >= 0){
        cout << s [n] <<endl;
        n--;
    }
}
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
// 6. 
bool pertenece(char c, string s){
    return perteneceAux (0,c,s)
}
bool perteneceAux (int i, char c, string s){
    if (s[i] == '\0'){
        return false;
    } else {
        return s [i] == c || perteneceAux (++i,c,s);
    }
}
// Propósito: indica si un char c aparece en el string s.
// 7. 
int apariciones(char c, string s){
    aparicionesAux (0,c,s);
}

int aparicionesAux (int n, char c, string s){
    if (s [i] == '\0'){
        return 0;
    } else {
        return unoSi (c == s[n]) + aparicionesAux (++n,c,s);
    }
}

int unoSi (bool s){
    if (true){
        return 1;
    } else {
        return 0;
    }
}
// Propósito: devuelve la cantidad de apariciones de un char c en el string s.

Si estas como implentador recorres los nodos. Los nodos estan en una estructura que sea recursiva 