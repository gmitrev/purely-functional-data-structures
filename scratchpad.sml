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
  val insert: Elem * Set -> Set
  val member: Elem * Set -> bool
end

datatype Tree = Leaf | Branch of Tree * int * Tree;

fun member(x, Leaf) = false
  | member(x, Branch (a, y, b)) =
    if x < y then member(x, a)
    else if x > y then member(x, b)
    else true;

val t1 = Branch(Leaf, 3, Branch(Leaf, 5, Leaf));

member(5, t1)


