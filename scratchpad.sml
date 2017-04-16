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
