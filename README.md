# SystemE

Implementation of a simple version of [System E](todohere)

# Types

System E generally uses commutative and associative (but not idempotent) intersection types with the empty intersection marked as $\omega$.
This is because we want to monitor how many times each variable was used.  
So for example if $` x: \alpha \cap \alpha \vdash M `$, then we know that $x$ is used twice  
in the term $M$.   

System E also includes expansion variable decorators into the type constructors: $` e \, \tau `$.   
But they don't increase power of the system.   
They assist in inferring principal typings for a term.   

# Usage 

For now only through ghci. So, in project directory run `stack ghci`.   
Main function is `infer`, it accepts term and returns typing of this term.  
You can construct terms using term syntax in `Format.hs` file. You can use predefined terms in `Main.hs` as examples.  
Basic example:
```shell
ghci> infer $ App (Lam xvar $ Lam yvar $ x) (Lam xvar $ x)
[] :- (ω -> (b -> b))
```
