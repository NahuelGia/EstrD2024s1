#include <string>
#include <iostream>
#include "LinkedList2.h"

using namespace std;

LinkedList nil(){
    LinkedList newList = new LinkedListSt;
    newList->cantidad = 0;
    newList->primero = NULL;
    newList->ultimo  = NULL;

    return newList;
}

bool isEmpty(LinkedList xs){
    return xs->cantidad == 0 ;
}

int head(LinkedList xs){
    return xs->primero->elem;
}

void Cons(int x, LinkedList xs){ 
    NodoL* nodo = new NodoL; 
    nodo->elem = x;
    nodo->siguiente = xs->primero;
    xs->primero = nodo;

    if (xs->ultimo == NULL)
    {
        xs->ultimo = nodo;
    }

    xs->cantidad++;
}

void Tail(LinkedList xs){
    if (xs->cantidad > 0){
        NodoL* NodoAEliminar = xs->primero;
        xs->primero = NodoAEliminar->siguiente;
        xs->cantidad--;
        delete NodoAEliminar;
    }

    if (xs->primero == NULL)
    {
        xs->ultimo == NULL;
    }
}


int length(LinkedList xs){
    return xs->cantidad;
}

// Costo O(n) donde n es la cantidad de nodos de la lista 
void Snoc(int x, LinkedList xs){

    NodoL* nuevo = new NodoL;
    nuevo->elem = x;
    nuevo->siguiente = NULL;

    if (xs->ultimo == NULL){
        xs->primero = nuevo;
    } else {
        xs->ultimo->siguiente = nuevo;
    }
    xs->ultimo = nuevo;    
    xs->cantidad++;
}

// Apunta el recorrido al primer elemento
ListIterator getIterator(LinkedList xs){
    ListIterator iterator = new IteratorSt;
    iterator->current = xs->primero;
    return iterator;    
}   

int current(ListIterator ixs){
    return ixs->current->elem;
}


void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem = x;
}

// Precond: No está al final del recorrido
void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs){
    return ixs->current == nullptr;
}

void DisposeIterator(ListIterator ixs){
    delete ixs;
}

void DestroyL(LinkedList xs){
    
    NodoL* actual = xs->primero;
    NodoL* aBorrar;

    while (actual != nullptr)
    {   
        aBorrar = actual;
        actual = actual->siguiente;
        delete aBorrar;
    }
    delete xs;
}

void Append(LinkedList xs, LinkedList ys){

    if (xs->ultimo == NULL){
        xs->primero = ys->primero;
        xs->ultimo = ys->primero;
    } else {
        xs->ultimo->siguiente = ys->primero;
    }

    xs->cantidad += ys->cantidad;
    // DestroyL(ys); //! No puedo borrar sus nodos, ya que se estos punteros ahora los tiene xs
    delete ys;
}