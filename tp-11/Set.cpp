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
    
    NodoS actual = s->primero;

    while (actual != NULL && actual.elem != x)
    {
        actual = actual.siguiente;
    } // Encontró al elemento o no quedan elementos por recorrer

    return actual != NULL;
}

void AddS(int x, Set s){ //! Revisar una mejor manera
    
    if (!belongsS(x,s))
    {
        NodoS* nuevo = new NodoS;
        nuevo->elem = x;
        nuevo->siguiente = s->primero;

        s->primero = nuevo;
        s->cantidad ++;
    }

}


