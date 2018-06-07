fun zipRecycle(list1:int list,list2:int list)=
    let
	fun auxrec(sub1:int list,sub2:int list,end1:bool,end2:bool)=
	    if end1 andalso end2
	    then []
	    else if null sub1
	    then auxrec(list1,sub2,true,end2)
	    else if null sub2
	    then auxrec(sub1,list2,end1,true)
	    else
		(hd sub1,hd sub2)::auxrec(tl sub1,tl sub2,end1,end2)
    in
	auxrec(list1,list2,false,false)
    end

fun zipOpt(list1:int list,list2:int list)=
    if (not(null list1) andalso null list2) orelse (null list1 andalso not(null list2))
    then NONE
    else if null list1 andalso null list2
    then SOME []
    else
	let
	    val rest=zipOpt(tl list1,tl list2)
	in
	    if not (isSome rest)then NONE else SOME ((hd list1,hd list2)::(valOf rest))
	end

fun all_products(factors:(int*int)list)=
    let
	fun auxunroll(factor:int*int)=
	    if #2 factor=0
	    then []
	    else (#1 factor)::auxunroll(#1 factor,#2 factor -1)
				       
	fun unroll(factors:(int*int)list)=
	    if null factors
	    then []
	    else auxunroll(hd factors)@unroll(tl factors)
					     
	val permu=unroll(factors)
			
	fun length(li:int list,len:int)=
	    if null li
	    then len
	    else length(tl li,len+1)
		       
	val len=length(permu,0)
		      
	fun dup(item:int,li:int list)=
	    if null li
	    then false
	    else if hd li=item
	    then true
	    else dup(item,tl li)
		    
	fun remove_duplicate(li:int list)=
	    if null li
	    then []
	    else if dup(hd li,tl li)
	    then remove_duplicate(tl li)
	    else (hd li)::remove_duplicate(tl li)

	fun generate1(prod:int,sublist:int list)=
	    let
		val sublist=remove_duplicate(sublist)
	    in
		if null sublist
		then []
		else (prod*hd sublist)::generate1(prod,tl sublist)
	    end
		
	fun jumpdup(li:int list,leap:int)=
	    if null li
	    then (~1,[],leap)
	    else if dup(hd li,tl li)
	    then jumpdup(tl li,leap+1)
	    else (hd li,tl li,leap)
			 		     
	fun generate2(prod:int,sublist:int list,remain:int,n:int)=
	    if null sublist orelse remain<n
	    then []
	    else if n=1
	    then generate1(prod,sublist)
	    else
		let
		    val next=jumpdup(sublist,0)
		    val head=(#1 next)
		    val rest=(#2 next)
		    val leap=(#3 next)
		in		
		    if head<>(~1)
		    then generate2(prod*hd sublist,tl sublist,remain-1,n-1)@
			 generate2(prod,rest,remain-1-leap,n)
		    else generate2(prod*hd sublist,tl sublist,remain-1,n-1)
		end

	fun merge(list1:int list,list2:int list)=
	    if null list1
	    then list2
	    else if null list2
	    then list1
	    else
		let
		    val h1=hd list1
		    val h2=hd list2
		    val dif=h1-h2
		in
		    if dif>0
		    then (hd list2)::merge(list1,tl list2)
		    else if dif=0
		    then (hd list1)::merge(tl list1,tl list2)
		    else (hd list1)::merge(tl list1,list2)
		end

	fun generateandmerge(n:int)=
	    if n=0
	    then [1]
	    else merge(generateandmerge(n-1),generate2(1,permu,len,n))

    in
	generateandmerge(len)
    end

fun exp(base : int, exponent : int) =
    if exponent = 0 then 1
    else base * exp(base, exponent - 1);

fun multiply (flist : (int * int) list) =
    if null flist then 1
    else exp(#1 (hd flist), #2 (hd flist)) * multiply(tl flist);

fun simpler_all_products (list : (int * int) list) =
    let
	val number = multiply(list)

	fun all_divisors_for (n : int, divisor : int) =
	    if divisor = 0 then []
	    else
    		if n mod divisor = 0
    		then all_divisors_for(n, divisor - 1) @ [divisor]
    		else all_divisors_for(n, divisor - 1)
    in
	all_divisors_for(number, number)
    end
