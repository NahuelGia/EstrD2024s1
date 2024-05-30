#include <string>
#include <iostream>
#include "Persona.h"

using namespace std;

Persona consPersona(string nombre, int edad){
    Persona p = new PersonaSt;

    p->nombre = nombre;
    p->edad   = edad;

    return p;
}

string nombre(Persona p){
    return p->nombre;
}

int edad(Persona p){
    return p->edad;
}

void crecer(Persona p){
    p->edad += 1;
}

void cambioDeNombre(string nombre, Persona p){
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2){
    return p1->edad > p2->edad ;
}

// Observación: Si tienen la misma edad devuelve la segunda
Persona laQueEsMayor(Persona p1, Persona p2){
   if (p1->edad > p2->edad)
   {
    return p1;
   } else
   {
    return p2;
   }
}

int main()
{
    /* code */
    return 0;
}
