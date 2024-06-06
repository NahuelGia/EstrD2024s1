#include <string>
#include <iostream>
#include "ArrayList.cpp"

using namespace std;

/* EJERCICIO 4*/

int sumatoria(ArrayList xs){

    int result = 0;

    for (int i = 0; i < lengthAL(xs); i++)
    {
        result += get(i, xs);
    }
    
    return result;
}

void sucesores(ArrayList xs){

    for (int i = 0; i < lengthAL(xs); i++)
    {
        set(i, get(i, xs) + 1, xs);
    }
}

bool pertenece(int x, ArrayList xs){ //? Chequear
    for (int i = 0; i < lengthAL(xs); i++)
    {
        if (get(i,xs) == x )
        {
            return true;
        }
    }
    return false;
}

int apariciones(int x, ArrayList xs){
    int result = 0;
    for (int i = 0; i < lengthAL(xs); i++)
    {
        if (get(i,xs) == x )
        {
            result ++;
        }
    }
    return result;
}

ArrayList append(ArrayList xs, ArrayList ys){ //? Preguntar

    ArrayList nueva = newArrayListWith(lengthAL(xs) + lengthAL(ys));

     for (int i = 0; i < lengthAL(xs); i++)
    {
       add(get(i,xs), nueva);
    }

     for (int i = 0; i < lengthAL(ys); i++)
    {
        add(get(i,ys), nueva);
    }

    return nueva;
}

int minimo(ArrayList xs){
    int min = get(0,xs);

    for (int i = 1; i < lengthAL(xs); i++)
    {
       min = min < get(i,xs) ? min : get(i,xs);
    }
    
    return min;
}


int main(){
    return 0;
}