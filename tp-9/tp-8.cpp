#include <iostream>

//* EJERCICIO 3

using namespace std;

typedef struct {
    int x;
    int y;
} Par;


// Propósito: construye un par
Par consPar(int x, int y){
    Par newPar;
    newPar.x = x; 
    newPar.y = y;
    return newPar;
}

// Propósito: devuelve la primera componente
int fst(Par p){
    return p.x;
}

// Propósito: devuelve la segunda componente
int snd(Par p){
    return p.y;
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p){
    return (p.x > p.y) ? p.x : p.y ; //* Forma acortada de un else if (condición ? valor_si_verdadero : valor_si_falso)
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p){
    Par newPar;
    newPar.x = p.x;
    newPar.y = p.y;
    return newPar;
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
// Precondición: El divisor no es cero. 
Par divisionYResto(int n, int m){
    Par newPar;
    newPar.x = n / m ;
    newPar.y = n % m ;
    return newPar;
}

//* EJERCICIO 4

// 1 
//Propósito: imprime n veces un string s.

void printNI(int n, char* s){
    while (n>0)
    {
        std::cout << s << std::endl ; //? Consultar como imprimir en pantalla correctamente
        n--;
    }
}

void printNR(int n, char* s){
    printf(s);
    if (n>0)
    {
        printNR(n-1, s);
    }  
}

// 2

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.

void cuentaRegresivaI(int num){
    while (num>0)
    {
        printf("%d\n",num);
        num--;
    }
}

void cuentaRegresivaR(int num){
  printf("%d\n",num);
  if (num>=0)
  {
    cuentaRegresivaR(num-1);
  }
}

// 3 

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea

void desdeCeroHastaNI(int n){ //? Que pasa si n es menor a cero 

    for (int i = 0; i <= n; i++) {
        std::cout << i << std::endl;
    }   
}

void desdeCeroHastaNR(int n){ //! Completar
    
}

// 4 

// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).

int mult(int n, int m){ //? Que pasa si hay números negativos 
    
    int resultado = 0;

    for (int i = 0; i < n; i++)
    {
        resultado += m;
    }

    return resultado;
}

int max(int n, int m){
    if (n>m)
    {
        return n;
    } else {
        return m;
    } 
}

int min(int n, int m){
    if (n<m)
    {
        return n;
    } else {
        return m;
    } 
}

// 5 

// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.

void primerosNI(int n, string s){

   for (int i = 0; i < n ; i++)
   {
    std::cout << s[i] << std::endl;
   }
   
}

// 6 


// Propósito: indica si un char c aparece en el string s.

bool perteneceI(char c, string s){

   int i = 0;

   while ( s[i] != c && s[i] != 0 )
   {
        i += 1;
   }
   return s[i] == c ;
}

// 7

// Propósito: devuelve la cantidad de apariciones de un char c en el string s.

int apariciones(char c, string s){

   int apariciones    = 0;

//   int indice = 0;

//    while ( s[i] != c && s[i] != null )
//    {
//         indice += 1;
//         apariciones += static_cast<int>(s[i] == c);
//    }
   
   for (int i = 0; i <= s.size() ; i++)
   {
        apariciones += static_cast<int>(s[i] == c);
   }
   
   return apariciones;

}

//* EJERCICIO 5 

struct Fraccion {
    int numerador;
    int denominador;
};

typedef struct Fraccion Fraccion;

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion newF;
    newF.numerador   = numerador;
    newF.denominador = denominador;

    return newF;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return f.numerador / f.denominador ;
}


// Precondición: Los números son mayores a cero
int mcdEntre(int x, int y){

    int aux;

    if (x>y){
        aux = x;
    } else
    {
        aux = y;
    }
    
    while ( aux > 0 && ((x % aux) != 0 || (y % aux) != 0) ){
        aux --;
    }

    return aux;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    int mcd = mcdEntre(p.denominador, p.numerador);

    Fraccion resultante;

    resultante.denominador = p.denominador / mcd;

    resultante.numerador = p.numerador / mcd;

    return resultante;
}

// // Propósito: devuelve la primera componente
// Fraccion sumF(Fraccion f1, Fraccion f2); //? No lo entiendo


void imprimirF(Fraccion f){
    int numerador = f.numerador;
    int denominador = f.denominador;
    std::cout << numerador << " / " << denominador << std::endl;
}

Fraccion fraccionEjemplo = consFraccion(4,2);

int main(){
    
    //imprimirF(fraccionEjemplo);
    std::cout << mcdEntre(-9,-8) << std::endl;

    return 0;
}