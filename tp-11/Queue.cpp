#include "Queue.h"

Queue emptyQ(){
    Queue q = new QueueSt;
    q->cantidad = 0;
    q->primero = nullptr;
    q->ultimo = nullptr;

    return q;
}

bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}

int firstQ(Queue q){
    return q->primero->elem;
}

void Enqueue(int x, Queue q){

    NodoQ* nodo = new NodoQ;
    nodo->elem = x;
    nodo->siguiente = nullptr;

    if (q->primero == nullptr) 
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
void Dequeue(Queue q){ 
    NodoQ* borrar = q->primero;

    q->primero = q->primero->siguiente;

    if (q->primero = nullptr)
    {
        q->ultimo  = nullptr; 
    } 
    delete borrar;
    q->cantidad--;
}

int lengthQ(Queue q){
    return q->cantidad;
}

void MergeQ(Queue q1, Queue q2){

    if (q1->ultimo == nullptr) // q1 está vacío
    {
        q1->primero = q2->primero;
        q1->ultimo  = q2->ultimo;
    } else {
        q1->ultimo->siguiente = q2->primero;
    }

    if (q2->ultimo != nullptr) 
    {
        q1->ultimo = q2->ultimo;
    }
    
    q1->cantidad += q2->cantidad;
    delete q2;
}

void DestroyQ(Queue q){
    NodoQ* actual = q->primero;
    NodoQ* borrar;

    while (actual != nullptr)
    {
        borrar = actual;
        actual = actual->siguiente;
        delete borrar;
    }

    delete q;
}
