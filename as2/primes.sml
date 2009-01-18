(*
I pledge my Honor that I have not cheated on this assignment.
- Zachary Carter
*)

open TextIO;
open List;

let

fun printList(nil) = ()
 |  printList(x::xs) = (
    print(Int.toString(x));
    print(" ");
    printList(xs)
    )

fun readInts(file) = 
      let
        val n = valOf(Int.fromString(valOf(inputLine(file))));
      in
        if n < 2 then
          nil
        else
          n::readInts(file)
      end
      
fun getPrimeFactors(n) = 
  let
    fun inc(x, m) = if x <= m then x::inc(x+1, m) else []
    val pList = inc(2, n)
    
    val r = floor(Real.Math.sqrt(real(n)))
    
    val sList = filter (fn x => x <= r ) pList
    
    fun iter(j, i, nList) = if j <= n then iter(j+1, i, filter (fn x =>  not(x mod (i*j) = 0)) nList) else nList

    fun remMultiples(i::mList, newList) = 
      remMultiples(mList, iter(2, i, newList) )
      | remMultiples(nil, newList) = newList
      
    val primeList = remMultiples(sList, pList)
    
    fun factors(x::List) = if n mod x = 0 then x::factors(List) else factors(List)
      | factors(nil) = []
      
  in
    factors(primeList)
  end
  
val L = readInts(stdIn)

val L2 = map (fn x => getPrimeFactors(x)) L

val p = print("=============================")
val p = print("\nPrime Factors:\n")

val p = foldl (fn (x,y) => (printList(x);print("\n");[])) [] L2

(* foldl takes a fn (element, result of last fold) *)

fun iterUniq(p, u::uniq, nList) = if p = u then nList else iterUniq(p, uniq, nList)
  | iterUniq(p, nil, nList) = p::nList
  
fun doWork(x::List, uniq) = doWork(List, iterUniq(x, uniq, uniq))
  | doWork(nil, uniq) = uniq
  
val L3 = foldl (fn (e,r) => doWork(e,r)) [] L2

val p = print("=============================")
val p = print("\nUnique prime factor list:\n")
val p = foldl (fn (x,y) => (print(Int.toString(x)^" ");[])) [] L3

(* filter our input, check if each one is in unique prime factor list *)
val L4 = filter (fn x => isSome( find (fn i => i = x) L3 ) ) L

val p = print("\n=============================")
val p = print("\nPrime numbers entered:\n")
val p = foldl (fn (x,y) => (print(Int.toString(x)^" ");[])) [] L4

in
  print("\n=============================\n")
end
