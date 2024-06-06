#include <string>
#include <iostream>
#include "ArrayList.h"

using namespace std;

ArrayList newArrayList(){
    
    ArrayList list = new ArrayListSt;
    list->cantidad  = 0; 
    list->capacidad = 16;
    list->elementos = new int[list->capacidad]; 
    return list;
}

ArrayList newArrayListWith(int capacidad){  
    
    ArrayList list = new ArrayListSt;
    list->cantidad  = 0;
    list->capacidad = capacidad > 0 ? capacidad : 16 ; //? Que pasa si me mandan una capacidad negativa
    list->elementos = new int[list->capacidad];
    return list;
}

int lengthAL(ArrayList xs){
    return xs->cantidad;
}

int get(int i, ArrayList xs){

    return (i >= 0 && i < xs->cantidad) ? xs->elementos[i] : 0 ;

    // return xs->elementos[i]; //? Si no existe?
}

void set(int i, int x, ArrayList xs){
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs){ //! Revisar 

    int* newArray = new int[capacidad]; //? Si capacidad es negativa?

    for (int i = 0; i < xs->cantidad && i < capacidad ; i++) //! Tiene que cortar cuando tiene todos los elementos 
    {                                                       //! o si ya llenó la nueva capacidad
        newArray[i] = xs->elementos[i]; 
    }

    delete[] xs->elementos; // Elimina el puntero al viejo array

    xs->elementos = newArray;
    
    if ( capacidad < xs->cantidad ){
        xs->cantidad = capacidad;
    }

    xs->capacidad = capacidad;

}

void add(int x, ArrayList xs){ 

    if (xs->cantidad == xs->capacidad)
    {
        resize(xs->capacidad*2, xs); //? Chequear 
    } 

    xs->elementos[xs->cantidad] = x;
    xs->cantidad += 1;
}

void remove(ArrayList xs){
    if (xs->cantidad > 0){
        xs->cantidad--;
    }
}

