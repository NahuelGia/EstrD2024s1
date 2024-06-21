struct NodoQ {
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt {
    // INV.REP:
    // - Cantidad es el nro de nodos que se pueden recorrer hasta llegar a NULL
    // - Si primero es NULL, entonces ultimo también lo es
    // - Si ultimo es NULL, entonces primero también lo es
    // - El siguiente elemento del nodo ultimo es NULL
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};

typedef QueueSt* Queue; // INV.REP: EL puntero no es NULL

// Crea una cola vacía.
// Costo: O(1).
Queue emptyQ();
// Indica si la cola está vacía.
// Costo: O(1).
bool isEmptyQ(Queue q);
// Devuelve el primer elemento.
// Costo: O(1).
int firstQ(Queue q);
// Agrega un elemento al final de la cola.
// Costo: O(1).
void Enqueue(int x, Queue q);
// Quita el primer elemento de la cola.
// Costo: O(1).
void Dequeue(Queue q);
// Devuelve la cantidad de elementos de la cola.
// Costo: O(1).
int lengthQ(Queue q);
// Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
// Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
// Costo: O(1).
void MergeQ(Queue q1, Queue q2);
// Libera la memoria ocupada por la cola.
// Costo: O(n).
void DestroyQ(Queue q);