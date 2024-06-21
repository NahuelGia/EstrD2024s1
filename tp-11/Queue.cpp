#include "Queue.h"

Queue emptyQ(){
    Queue q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;

    return q;
}

bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}

int firstQ(Queue q){
    return q->primero;
}

void Enqueue(int x, Queue q){

    NodoQ* nodo = new NodoQ;
    nodo->elem = x;
    nodo->siguiente = NULL;

    if (q->primero == NULL) // Es decir la Queue está vacia 
    {
        q->primero = nodo;
    } else
    {
        q->ultimo->siguiente = nodo;
    }

    q->ultimo = nodo;
    q->cantidad++;
}

// Precond: La queue tiene al menos un elemento
void Dequeue(Queue q){ //! Revisar
    NodoQ* borrar = q->primero;

    if (q->cantidad = 1)
    {
        q->primero = NULL;
        q->ultimo  = NULL; 
    } else {
        q->primero = q->primero->siguiente;
    }
    
    delete borrar;
    q->cantidad--;

}
