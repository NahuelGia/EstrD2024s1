#include <string>
#include <iostream>
#include "Set.h"

using namespace std;

Set emptyS(){
    Set vacio = new SetSt;
    vacio->cantidad = 0;
    vacio->primero = NULL;

    return vacio;
}

bool isEmptyS(Set s){
    return s->cantidad == 0;
}

bool belongsS(int x, Set s){
    
    NodoS* actual = s->primero;

    while (actual != NULL && actual->elem != x)
    {
        actual = actual->siguiente;
    } // Encontró al elemento o no quedan elementos por recorrer

    return actual != NULL;
}

void AddS(int x, Set s){
    
    if (!belongsS(x,s))
    {
        NodoS* nuevo = new NodoS;
        nuevo->elem = x;
        nuevo->siguiente = s->primero;

        s->primero = nuevo;
        s->cantidad ++;
    }

}

void RemoveS(int x, Set s){

    NodoS* anterior = NULL;
    NodoS* actual   = s->primero;
    
    while (actual != NULL && actual->elem != x)
    {   
        anterior = actual;
        actual = actual->siguiente;
    } // Encontré al elemento o llegué al final del Set 
    
    if (actual != NULL) // Si es NULL no encontré al elemento 
    {
        if (anterior == NULL)
        {
            s->primero = NULL;
        } else {
            anterior->siguiente = actual->siguiente; 
        }
        delete actual;
        s->cantidad--;
    }
}

int sizeS(Set s){
    return s->cantidad;
}

LinkedList setToList(Set s){

    NodoS* actual = s->primero;
    LinkedList list = nil();

    while (actual != NULL)
    {
        Cons(actual->elem, list);
        actual = actual->siguiente;
    }

    return list;

}

void DestroyS(Set s){

    NodoS* actual = s->primero;
    NodoS* borrar;

    while (actual != NULL)
    {
        borrar = actual;
        actual = actual->siguiente;
        delete borrar;
    }

    delete s;
}




