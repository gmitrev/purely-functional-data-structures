use "chapter2.sml";

signature Ordered = sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature Heap = sig
  structure Elem : Ordered

  type Heap

  val empty     : Heap
  val isEmpty   : Heap -> bool

  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap

  val findMin   : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end

functor LeftistHeap(Element : Ordered) : Heap =
struct
  structure Elem = Element

  datatype Heap = E | T of int * Elem.T * Heap * Heap

  val empty = E

  fun isEmpty E = true
    | isEmpty _ = false

  fun rank E = 0
    | rank(T(r, _, _, _)) = r

  fun makeT(x, a, b) =
    if rank(a) >= rank(b) then T(rank(b) + 1, x, a, b)
    else T(rank(a) + 1, x, b, a)

  fun merge(h, E) = h
    | merge(E, h) = h
    | merge(h1 as T(_, x, a1, b1), h2 as T(_, y, a2, b2)) =
      if Elem.leq(x, y) then makeT(x, a1, merge(b1, h2))
      else makeT(y, a2, merge(h1, b2))

  fun insert(x,h) = merge(T(1, x, E, E), h)

  fun findMin(E) = raise Empty
    | findMin(T(_, x, a, b)) = x

  fun deleteMin(E) = raise Empty
    | deleteMin(T(_, x, a, b)) = merge(a, b)
end
