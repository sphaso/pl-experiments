# Programming Language Experiments
Experiments from the book "Essentials of Programming Languages" by Friedman and Wand    

LET - parser missing    
PROC - parser missing    
LETREC - skipped, since PROC already includes recursion    
PROC (DeBruijn) - Added indices to PROC, finally hacked a currying-working implementation but recursion is broken and not really sure how to implement it without using a Y combinator    
ExplicitRefs - parser missing    
ImplicitRefs - skipped, didn't seem a major step forward from ExplicitRefs    
Continuation Passing Style (CPS) - still have to fix recursion   
Trampolines - skipped, didn't see the point of implementing it in Haskell    
THREAD - skipped, I would need to implement ImplicitRefs and Trampolines first
Checked - only LetRec implementation is missing since I didn't implement LETREC in the first place
Inferred - 
