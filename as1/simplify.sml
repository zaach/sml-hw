(*I pledge my Honor that I have not cheated on this assignment.
- Zachary Carter *)

fun simplify(polynom) = 
  let
    (* declare these just to make calls more clear, kinda *)
    val templist = []
    val constant = (0,#"c",0)
    
    (* basic selection-like sort *)
    fun insert (nil, ins, NNL) = NNL@[ins]
      | insert ((tup as (_,_,exp))::NList, ins as (_,_,exp2), NNL) = 
        if exp2 > exp then
          NNL@ins::tup::NList
        else
          insert(NList, ins, NNL@[tup]);
        
    fun reorder (nil, NL) = NL
      | reorder (tup::List, NL) = 
        reorder(List, insert(NL, tup, templist));
        
    (* combine all zero-ed exponent *)
    fun zeros ((tup as (con,_,exp))::List, comb as (con2,_,exp2), NL) =
      if exp = 0 then
        zeros(List, (con2+con,#"c", 0), NL)
      else
        zeros(List, comb, tup::NL)
      | zeros (nil, comb, NL) = comb::NL;
      
    (* see if polynomial of same var and exponent is already in the list
       and combine it *)
    fun addtups ((tup as (c, v, e))::NList, comb as (c2, v2, e2), NNL) =
        if v = v2 andalso e = e2 then
          (c+c2, v, e2)::NList@NNL
        else
          addtups(NList, comb, tup::NNL)
      | addtups (nil,comb, NNL) = comb::NNL;
      
    (* combine polynomials of like vars and exponents *)
    fun combine (tup::List, NL) =
        combine(List, addtups(NL,tup,templist) )
      | combine (nil, NL) = NL;
  in
    reorder(combine(zeros(polynom, constant, templist), templist), templist)
  end;
