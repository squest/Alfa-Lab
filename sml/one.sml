open List;
open Math;

fun sqr x = x * x;

fun div' (a : int) (b : int) = 0 = (a mod b);

fun prime' (m:int) =
    let val lim = Real.ceil (sqrt (Real.fromInt m))
	fun helper (i:int) =
	    if i > lim
	    then true
	    else if m mod i = 0
	    then false
	    else helper (i + 2)
    in helper 3
    end;

fun suma_prima (lim:int) =
    let fun helper (i:int) (res : LargeInt.int) =
	    if i > lim
	    then res
	    else if prime' i
	    then helper (i + 2) (Int.toLarge i + res)
	    else helper (i + 2) res
    in helper 7 10
    end;

fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = suma_prima x
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;
