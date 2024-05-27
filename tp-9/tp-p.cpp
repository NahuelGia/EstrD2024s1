#include <string>
#include <iostream>

using namespace std;

/* EJERCICIO 1 */

struct Persona
{
    string nombre;
    int    edad;
};

typedef Persona* PersonaPtr;


PersonaPtr consPersona(string nombre, int edad){
    PersonaPtr p = new Persona;

    p->nombre = nombre;
    p->edad   = edad;

    return p;
}

string nombre(PersonaPtr p){
    return p->nombre;
}

int edad(PersonaPtr p){
    return p->edad;
}

void crecer(PersonaPtr p){
    p->edad += 1;
}

void cambioDeNombre(string nombre, PersonaPtr p){
    p->nombre = nombre;
}

bool esMayorQueLaOtra(PersonaPtr p1, PersonaPtr p2){
    return p1->edad > p2->edad ;
}


// Observación: Si tienen la misma edad devuelve la segunda
PersonaPtr laQueEsMayor(PersonaPtr p1, PersonaPtr p2){
   if (p1->edad > p2->edad)
   {
    return p1;
   } else
   {
    return p2;
   }
}

/* EJERCICIO 2 */

typedef string TipoDePokemon;

struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

typedef PokeSt* Pokemon;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

typedef EntrenadorSt* Entrenador;

Pokemon consPokemon(TipoDePokemon tipo){
    Pokemon p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipo(Pokemon p){
    return p->tipo;
}

int energia(Pokemon p){
    return p->vida;
}

void perderEnergia(int energia, Pokemon p){
    p->vida -= energia;
}

bool superaA(Pokemon p1, Pokemon p2){
    return (   (p1->tipo == "agua"   && p2->tipo == "fuego" )
            || (p1->tipo == "fuego"  && p2->tipo == "planta")
            || (p1->tipo == "planta" && p2->tipo == "agua")
           );
}

/* Entrenador */ //! Pasar bien las interfaces a sus archivos correspondientes


Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){ //? pokemon es una array o un solo pokemon?
    Entrenador s   = new EntrenadorSt;
    s->nombre      = nombre;
    s->cantPokemon = cantidad;
    s->pokemon     = pokemon; //* Chequear si esto es un array o si lo tengo que crear 
    return s;
}



int main(){
    return 0;
}