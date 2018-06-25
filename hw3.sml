(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun currysub x y=String.sub(y,x)
val only_capitals = List.filter (Char.isUpper o (currysub 0))

fun cmp1(x,y)=
    if String.size x > String.size y
    then x
    else y

val longest_string1= List.foldl cmp1 ""

fun cmp2(x,y)=
    if String.size x >= String.size y
    then x
    else y

val longest_string2= List.foldl cmp2 ""

fun longest_string_helper f =
    let fun cmp(x,y)=
	    if f(String.size x,String.size y)
	    then x
	    else y
    in List.foldl cmp "" end

val longest_string3=longest_string_helper (op >)
val longest_string4=longest_string_helper (op >=)

val longest_capitalized=longest_string1 o only_capitals

val rev_string=String.implode o rev o String.explode

fun first_answer f ls=
    case ls of
	[]=>raise NoAnswer
      | a::b => (case f a of
		     SOME v => v
		   | NONE => first_answer f b)

fun all_answers f ls=
    let
	fun helper(ls,acc)=
	    case ls of
		[]=>SOME acc
	      | a::b => (case f a of
			     SOME lst => helper(b,lst@acc)
			   | NONE => NONE)
    in helper(ls,[]) end

val count_wildcards=g (fn()=>1) (fn x=>0)
val count_wild_and_variable_lengths=g (fn()=>1) String.size
fun count_some_var(str,p)=g (fn()=>0) (fn x=>if x=str then 1 else 0) p

fun check_pat p=
    let
	fun getstrs(p,acc)=
	    case p of
		Variable x        => x::acc
	      | TupleP ps         => foldl getstrs acc ps
	      | ConstructorP(_,p) => getstrs (p,acc)
	      | _                 => acc

	fun distinct strs=
	    case strs of
		[]=>true
	      | a::b => not(List.exists (fn x=>x=a) b) andalso distinct b
    in (distinct o getstrs)(p,[]) end

fun match(va,pa)=
    case (va,pa) of
	(_,Wildcard)=>SOME []
      | (v,Variable s) => SOME [(s,v)]
      | (Unit,UnitP) => SOME []
      | (Const v,ConstP p) => if p=v then SOME [] else NONE
      | (Tuple vs,TupleP ps) => if length vs = length ps
				then all_answers match (ListPair.zip(vs,ps))
				else NONE
      | (Constructor(s2,v),ConstructorP(s1,p)) => if s1=s2 then match(v,p) else NONE
      | _ => NONE

fun first_match v ps=
    SOME(first_answer (fn p=>match(v,p)) ps) handle NoAnswer => NONE

							      
fun typecheck_patterns(cons,ps)=
    let
	fun traverse f acc ps=
	    case ps of
		[]=>SOME acc
	      | p::ps => (case f p acc of
			      NONE=>NONE
			    | SOME a => traverse f a ps)

	fun finddatatype s=
	    case List.find (fn (a,b,c)=>a=s) cons of
		SOME (_,x,dt)=>SOME (x,dt)
	      | NONE => NONE

	fun compat(dt,pt)=
	    case (dt,pt) of
		(Anything,_)=>true
	      | (UnitT,UnitP) => true
	      | (TupleT dts,TupleP pts) =>
		length dts=length pts andalso not(List.exists (not o compat) (ListPair.zip(dts,pts)))
	      | (IntT,ConstP _) => true
	      | (Datatype s,ConstructorP (s1,pt)) =>
		(case finddatatype s1 of
		     SOME (x,dt)=>x=s andalso compat(dt,pt)
		   | NONE => false)
	      | (_,Variable _) => true
	      | (_,Wildcard) => true
	      | _ => false

	fun findt p =
	    let
		fun auxfindt(ps,acc)=
		    case ps of
			[]=>(SOME o rev) acc
		      | p::ps => case findt p of
				     NONE=>NONE
				   | SOME t => auxfindt(ps,t::acc)
	    in
		case p of
		    Wildcard=>SOME Anything
		  | Variable s => SOME Anything
		  | UnitP => SOME UnitT
		  | ConstP _ => SOME IntT
		  | TupleP ps =>
		    (case auxfindt(ps,[]) of
			 SOME ts=>(SOME o TupleT)ts
		       | NONE => NONE)
		  | ConstructorP (s,pt) =>
		    (case finddatatype s of
			 SOME (x,dt)=>if compat(dt,pt)
				      then SOME(Datatype x)
				      else NONE
		       | NONE => NONE)
	    end
		
	fun restrict(a,b)=
	    let
		fun auxres ls acc=
		    case ls of
			[]=>SOME acc
		      | (a,b)::rest => (case restrict(a,b) of
					    NONE=>NONE
					  | SOME x => auxres rest (x::acc))
	    in
		case a of
		    Anything=>SOME b
		  | UnitT => (case b of
				  UnitT=>SOME UnitT
				| Anything => SOME UnitT
				| _ => NONE)
		  | TupleT lsa => (case b of
				       Anything=>SOME a
				     | TupleT lsb => if length lsa=length lsb
						     then (case auxres (ListPair.zip(lsa,lsb)) []of
							      NONE=>NONE
							    | SOME x => (SOME o TupleT o rev) x)
						     else NONE
				     | _ => NONE)
		  | IntT => (case b of
				 Anything=>SOME a
			       | IntT => SOME a
			       | _ => NONE)
		  | Datatype s => (case b of
				       Anything=>SOME a
				     | Datatype s1 => if s=s1 then SOME a
						      else NONE
				     | _ => NONE)
	    end

	fun updatet p ori=
	    case findt p of
		NONE=>NONE
	      | SOME t => restrict(t,ori)
    in
	traverse updatet Anything ps
    end
