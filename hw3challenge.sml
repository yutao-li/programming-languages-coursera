fun foldl f acc ls=
    let
	fun h(x,g)=fn y=>g(f(x,y))
    in
	foldr h (fn x=>x) ls acc
    end
