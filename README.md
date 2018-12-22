# programming-languages-coursera
solutions to some extra challenging problems in course "programming languages"


## challenge1
function **factorize:int->(int*int)list** definition: given a number **n** returns a list of pairs **(d,k)** where **d** is a prime number dividing **n** and **k** is the number of times it fits. The pairs would be in increasing order of prime factor, and the process would stop when the divisor considered surpasses the square root of **n**.  
Write a function **all_products : (int * int) list -> int list** that given a factorization list result from **factorize** creates a list all of possible products produced from using some or all of those prime factors no more than the number of times they are available. This should end up being a list of all the divisors of the number **n** that gave rise to the list. Example: **all_products([(2,2),(5,1)])=[1,2,4,5,10,20]**. For extra challenge, your recursive process should return the numbers in this order, as opposed to sorting them afterwards.

## challenge2
Write a function typecheck_patterns that “type-checks” a pattern list. Types for our made-up pattern language are defined by:  
<pre>
datatype typ = Anything (* any type of value is okay *)  
             | UnitT (* type for Unit *)  
             | IntT (* type for integers *)  
             | TupleT of typ list (* tuple types *)  
             | Datatype of string (* some named datatype *)  
</pre>          
typecheck_patterns should have type ((string * string * typ) list) * (pattern list) -> typ option. The first argument contains elements that look like ("foo","bar",IntT), which means constructor foo makes a value of type Datatype "bar" given a value of type IntT. Assume list elements all have different first fields (the constructor name), but there are probably elements with the same second field (the datatype name). Under the assumptions this list provides, you “type-check” the pattern list to see if there exists some typ (call it t) that all the patterns in the list can have. If so, return SOME t, else return NONE. You must return the “most lenient” type that all the patterns can have. For example, given patterns TupleP[Variable("x"),Variable("y")] and TupleP[Wildcard,Wildcard], return TupleT[Anything,Anything] even though they could both have type TupleT[IntT,IntT]. As another example, if the only patterns are TupleP[Wildcard,Wildcard] and TupleP[Wildcard,TupleP[Wildcard,Wildcard]], you must return TupleT[Anything,TupleT[Anything,Anything]].

## challenge3
Implement **foldl** using **foldr** on functions. 

## challenge4
first refer to [https://www.coursera.org/learn/programming-languages-part-b/programming/xQBbK/homework-5-auto-grader] for MUPL programs specifications.  
Using lambda-calculus ideas to remove features from MUPL programs:

Like Racket itself, MUPL (the programming language from this section's homework assignment) is essentially a superset of untyped lambda calculus. "Lambda calculus" may sound scary, but it's essentially a very simple programming language -- it really doesn't have anything in it, apart from anonymous functions and function calls! Of course, that makes it very inconvenient to program in, which is also why real programming languages usually supply all sorts of bells and whistles, like additional language constructs and data types like booleans and numbers.

Nonetheless, untyped lambda calculus is Turing-complete, so we can actually represent things like numbers and booleans using nothing but functions. In these problems we'll do some of that in MUPL.

Notice that MUPL doesn't need **mlet**: Anywhere we have **mlet name e body**, we can use **call (fun #f name body) e** instead and get the same result. Write a Racket function **remove-lets** that takes a MUPL program and produces an equivalent MUPL program that does not contain **mlet**.
[more challenging] Now we will do something even more clever: remove pairs by using closure environments as another way to "hold" two pieces of data. Instead of using, **apair e1 e2**, we can use **mlet "_x" e1 (mlet "_y" e2 (fun #f "_f" (call (call (var "_f") (var "_x")) (var "_y")))))** (assuming **"_x"** isn't already used in **e2** -- we will assume that). This will evaluate to a closure that has the result of evaluating **e1** and **e2** in its environment. When the closure is called with a function, that function will be called with the result of evaluating **e1** and **e2** (in curried form). So if we replace every apair expression as described above, then we can, rather cleverly, replace **fst e** with **call e (fun #f "x" (fun #f "y" (var "x"))**. Extend your **remove-lets**, renaming it **remove-lets-and-pairs** so that it removes all uses of **apair**, **fst**, and **snd**. (We are leaving it to you to figure out how to replace **snd e**. Note 1: Remember you need to remove things recursively inside of **apair**, **fst**, etc., else an expression like **fst (snd (var "x"))** won't have the **snd** removed. Note 2: The resulting program should produce the same result when evaluated if the (original) result doesn't contain any pair values. If the original result does contain pair values, the result after removal will contain corresponding closures. Note 3: A slightly more challenging approach is to change how **apair** is removed so that we do not need to assume **"_y"** is not used in **e2**.
