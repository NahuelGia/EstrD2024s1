#include <string>
#include <iostream>
#include "Entrenador.h"

using namespace std;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){ //? pokemon es una array o un solo pokemon?
    Entrenador s   = new EntrenadorSt;
    s->nombre      = nombre;
    s->cantPokemon = cantidad;
    s->pokemon     = pokemon; //* Chequear si esto es un array o si lo tengo que crear 
    return s;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}

int unoSi(bool booleano){
    if (booleano)
    {
        return 1;
    } 
    return 0;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
 
    int cantidad = 0 ;
 
    for (int i = 0; i < e->cantPokemon ; i++)
    {
        cantidad += unoSi (e->pokemon[i]->tipo == tipo); //? Tengo que ser usuario de pokemon? La flecha hace que me meta en implementacion?
    }
    
    return cantidad;
}


// Precondición: existen al menos i − 1 pokémon.
Pokemon pokemonNro(int i, Entrenador e){
    return e->pokemon[i-1];
}

bool pokemonLeGanaATodos(Pokemon p, Pokemon* pks){
    
    
    for (int i = 0; i < sizeof(pks); i++) //? El sizeof se llama por cada elemento o solo una vez? Para el tema memoria
    {   
        if ( not superaA(p, pks[i]))
        {
            return false;
        }
    }

    return true;
}

bool leGanaATodos(Entrenador e1, Entrenador e2){ //? Ver si hay una forma mejor
    for (int i = 0; i < e1->cantPokemon ; i++)
    {
        if (pokemonLeGanaATodos(e1->pokemon[i], e2->pokemon))
        {
            return true;
        }
    }
    return false;
}

int main(){
    /* code */
    return 0;
}