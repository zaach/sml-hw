open Random;
val r = rand (IntInf.toInt (IntInf.mod (Time.toSeconds (Time.now ()), 65536)), 0);
fun generatePolynomial numTerms =
let 
  fun genTerm() =
    (randRange(1,6) r, 
     List.nth([#"x", #"y", #"z"], randRange(0,2) r), 
     randRange(0,4) r)
in
  if numTerms=0 then [] else (genTerm()) :: (generatePolynomial (numTerms-1))
end
;
