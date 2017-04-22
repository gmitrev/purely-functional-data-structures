(* ++ *)
fun ++([], ys) = ys
  | ++(x::xs, ys) = x :: ++(xs, ys);

++([1, 2], [3, 4]);

(* update *)
fun update([], x, y) = raise Subscript
  | update(x::xs, 0, y) = y :: xs
  | update(x::xs, i, y) = x :: update(xs, i - 1, y);

update([], 2, 5);
update([1,2,3], 1, 5);

(* ex 2.1 *)
fun suffixes([]) = [nil]
  | suffixes(x::xs) = (x::xs) :: suffixes(xs);

suffixes([1,2,3,4]);

(* BST *)

signature Set =
sig
  type Elem
  type Set

  val empty: Set
  val member: Elem * Set -> bool
  val insert: Elem * Set -> Set
end

datatype Tree = Leaf | Branch of Tree * int * Tree;

fun member(x, Leaf) = false
  | member(x, Branch (a, y, b)) =
    if x < y then member(x, a)
    else if x > y then member(x, b)
    else true;

val t1 = Branch(Leaf, 3, Branch(Leaf, 5, Leaf));

member(1, t1)
member(5, t1)

fun insert(x, Leaf) = Branch(Leaf, x, Leaf)
  | insert(x, tree as Branch(t1, y, t2)) =
    if x < y then Branch(insert(x, t1), y, t2)
    else if x > y then Branch(t1, y, insert(x, t2))
    else tree;

val tree = Branch(Leaf, 5, Leaf);
val tree = insert(2, tree);
val tree = insert(3, tree);
val tree = insert(6, tree);
val tree = insert(7, tree);

(* Ordered *)
signature Ordered = sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

functor UnbalancedSet (Element: Ordered): Set = struct
  type Elem = Element.T
  datatype Tree = Leaf | Branch of Tree * Elem * Tree
  type Set = Tree

  val empty = Leaf

  fun member(x, Leaf) = false
    | member(x, Branch(t1, y, t2)) =
      if Element.lt(x, y) then member(x, t1)
      else if Element.lt(y, x) then member(x, t2)
      else true

  fun insert(x, Leaf) = Branch(Leaf, x, Leaf)
    | insert(x, tree as Branch(t1, y, t2)) =
      if Element.lt(x, y) then Branch(insert(x, t1), y, t2)
      else if Element.lt(y, x) then Branch(t1, y, insert(x, t2))
      else tree
end
