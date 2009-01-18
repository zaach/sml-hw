(*I pledge my Honor that I have not cheated on this assignment.
- Zachary Carter *)

fun str2nats(str) = 
  let
    fun beginNat(nil, nlist) = nlist
      | beginNat(c::clist, nlist) = 
        if ord(c) < 48 orelse ord(c) > 57 then
          beginNat(clist, nlist)
        else
          endNat(clist, [c], nlist)
    and endNat(nil, n, nlist) = nlist@[valOf(Int.fromString(implode(n)))]
      | endNat(c::clist, n, nlist) =
        if ord(c) < 48 orelse ord(c) > 57 then 
            beginNat(clist, nlist@[valOf(Int.fromString(implode(n)))])
        else
          endNat(clist, n@[c], nlist)
  in
    beginNat(explode(str), [])
  end;
