#include "Tree.h"
#include <string>
#include <iostream>
#include "LinkedList.h"

using namespace std;

// EJERCICIO 2 
// 1

int sumatoria(LinkedList xs){
    ListIterator it = getIterator(xs);
    int result = 0 ;

    while (!atEnd(it))
    {
      result += current(it);
      Next(it);  
    }
    
    DisposeIterator(it);
    return result;
}

// 2 

void Sucesores(LinkedList xs){
    ListIterator it = getIterator(xs);

    while (!atEnd(it))
    {
        SetCurrent((current(it)+1), it);
        Next(it);
    }
    DisposeIterator(it);
}

// 3 

bool pertenece(int x, LinkedList xs){
    ListIterator it = getIterator(xs);

    while (!atEnd(it) && current(it) != x )
    {
        Next(it);
    } // Llegó al final o lo encontró

    bool encontrado = !atEnd(it);

    DisposeIterator(it);

    return encontrado ;
}

// 4 

int apariciones(int x, LinkedList xs){
    ListIterator it = getIterator(xs); 
    int vistos = 0;

    while (!atEnd(it))
    {
        vistos += current(it) == x ? 1 : 0 ;
        Next(it);
    }
    
    DisposeIterator(it);
    return vistos;
}

// 5 

// Precondicion: La lista tiene al menos un elemento
int minimo(LinkedList xs){
    ListIterator it = getIterator(xs); 
    int minimo = current(it);
    Next(it);

    while (!atEnd(it))
    {
        if ( current(it) < minimo)
        {
            minimo = current(it);
        }
        Next(it);
    }

    DisposeIterator(it);

    return minimo;
}

// 6

// Dada una lista genera otra con los mismos elementos, en el mismo orden.
LinkedList copy(LinkedList xs){
    ListIterator it = getIterator(xs); 
    LinkedList list = nil();

    while (!atEnd(it))
    {
        Snoc(current(it), list);
        Next(it);
    }
    
    DisposeIterator(it);

    return list;
}
// Para que Snoc fuera O(1) la estructura deberia tener la capacidad de guardar el puntero
// hacia el último nodo. Y el costo de copy pasaría de ser O(n^2) a ser O(n) .
// Ya que se recorre solo la lista xs y por cada elemento de xs se hace Snoc de O(1)

// 7

// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye.

// O(n*m) Donde la primer n es la cantidad de elementos de ys por el recorrido del iterador iys
// Y luego por cada elemento de n se realiza Snoc de O(n) sobre la lista de elementos de xs 
void Append(LinkedList xs, LinkedList ys){

    ListIterator iys = getIterator(ys); 

    while (!atEnd(iys))
    {
        Snoc(current(iys), xs);
        Next(iys);
    }
    
    DisposeIterator(iys);
    DestroyL(ys);

}

/* Para que Snoc fuera O(1) la estructura deberia tener la capacidad de guardar el puntero
   hacia el último nodo. Para el caso en que Snoc fuera O(1) el costo de append pasaría de ser
   O(n*m) a ser O(n) ya que por cada elemento de ys se llama a Snoc .
 */


// EJERCICIO 7

//1

// O(n) donde n es la cantidad de elementos del árbol. Y también es n sobre la creación de stack frames.
int sumarT(Tree t){
    int valorActual = 0;

    if (!isEmptyT(t))
    {
        valorActual += rootT(t);
        valorActual += sumarT(left(t));
        valorActual += sumarT(right(t));
    }
    return valorActual;
}

//2

int sizeT(Tree t){
    int elemVistos = 0;

    if(!isEmptyT(t))
    {
        elemVistos ++;
        elemVistos += sizeT(left(t));
        elemVistos += sizeT(right(t));
    }

    return elemVistos;
}

//3

bool perteneceT(int e, Tree t){ // TODO comprobar funcionamiento

    if (!isEmpty(t) )
    {
        return rootT(t) == e || perteneceT(e, left(t)) || perteneceT(e, right(t));
    } 
    
    return false;
}

//4


int main()
{
    std::cout << "La lista está vacía: "<< std::endl;
    return 0;
}
