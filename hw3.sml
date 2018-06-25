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
