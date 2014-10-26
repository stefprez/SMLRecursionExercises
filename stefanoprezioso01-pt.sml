(* 
    Stefano Prezioso
    COSC 341 Assignment 1
    Pattern Style
*)

(*#1*)
(* FUNCTION NAME: dispnthc *)
(* DESCRIPTION: display the nth character of a String *)
fun dispnthc("", n:int) = chr(0)
    |   dispnthc(myString: string, 0) = chr(0)
    |   dispnthc(myString: string, 1) = hd(explode(myString))
    |   dispnthc(myString: string, n: int) = dispnthc(implode(tl(explode(myString))), n - 1);
    

(*#2*)
(* FUNCTION NAME: delnthc *)
(* DESCRIPTION: delete the nth character of a String *)
fun delnthc("", n:int) = ""
    |   delnthc(myString: string, 0) = myString
    |   delnthc(myString: string, 1) = implode(tl(explode(myString)))
    |   delnthc(myString: string, n: int) = str(hd(explode(myString))) ^ delnthc(implode(tl(explode(myString))), n - 1);

(*#3*)
(* FUNCTION NAME: multinhelper *)
(* DESCRIPTION: helper function for multin *)
fun multinhelper(value, multiplier, numOfTimes, 0) = value :: multinhelper(value, multiplier, numOfTimes, 1)
    |   multinhelper(value, multiplier, numOfTimes, counter) = if counter = numOfTimes then [value * multiplier] 
        else  (value * multiplier) :: multinhelper(value * multiplier, multiplier, numOfTimes, counter + 1);

(* FUNCTION NAME: multin *)
(* DESCRIPTION: take a list of three and create a list of elements a * b c times *)
fun multin(nil) = nil
    |   multin([a: int, b: int, 0]) = [a]
    |   multin([a: int, b: int, c: int]) = multinhelper(a, b, c, 0)
    |   multin(L) = nil;

(*#4*)
(* FUNCTION NAME: remv *)
(* DESCRIPTION: remove instances of n in the list L *)
fun remv(n, nil) = nil
    |   remv(n, x :: nil) = if x = n then nil else [x]
    |   remv(n, x :: xs) = if x = n then remv(n, xs) else x :: remv(n, xs);

(*#5*)
(* FUNCTION NAME: remvdub *)
(* DESCRIPTION: remove any repeated instances in the list L *)
fun remvdub(nil) = nil
    |   remvdub(x :: nil) = [x]
    |   remvdub(x :: xs) = x :: remvdub(remv(x, xs));

(*#6*)
(* FUNCTION NAME: int2str *)
(* DESCRIPTION: convert an integer to a numeric String *)
fun int2str(n:int) = 
    if n div 10 = 0 then str(chr((n mod 10) + 48)) 
        else int2str(n div 10) ^ str(chr((n mod 10) + 48));

(*#7*)
(* FUNCTION NAME: str2inthelper *)
(* DESCRIPTION: helper function for str2int *)
fun str2inthelper(n:int, nil) = 0
    |   str2inthelper(n:int, x :: nil) = (n * 10) + (ord(x) - 48)
    |   str2inthelper(n:int, x :: xs) = str2inthelper((n * 10) + (ord(x) - 48), xs);
        
(* FUNCTION NAME: str2int *)
(* DESCRIPTION: convert a numeric String to an int *)
fun str2int(n:string) = 
    if null(tl(explode(n))) 
        then (ord(hd(explode(n))) - 48) 
    else str2inthelper(ord(hd(explode(n))) - 48, tl(explode(n)));

(*#8*)
(* FUNCTION NAME: indehelper *)
(* DESCRIPTION: helper function for inde *)
fun indehelper(n, nil, counter: int) = nil
    |   indehelper(n, x :: xs, counter: int) = if x = n 
        then counter :: indehelper(n, xs, counter + 1)
        else indehelper(n, xs, counter + 1);
        
(* FUNCTION NAME: inde *)
(* DESCRIPTION: returns index of occurence of given value starting at 1 *)
fun inde(n, nil) = nil
    | inde(n, L) = indehelper(n, L, 1);

(*#9*)
(* FUNCTION NAME: occrhelper *)
(* DESCRIPTION: helper function for occr *)
fun occrhelper(n, nil, counter: int) = (n, counter)
    |   occrhelper(n, x :: xs, counter: int) = if n = x then occrhelper(n, xs, counter + 1)
        else occrhelper(n, xs, counter);
        
(* FUNCTION NAME: occr *)
(* DESCRIPTION: dislay the occruence of an element in a list *)
fun occr(nil) = nil
    |   occr(x :: xs) = occrhelper(x, xs, 1) :: occr(remv(x, xs));

(*#10*)
(* FUNCTION NAME: nelehelper *)
(* DESCRIPTION: helper function for nele *)
fun nelehelper(nil, n: int, n2: int) = nil
    |   nelehelper(x :: xs, 0, n2: int) = nelehelper(xs, n2, n2)
    |   nelehelper(x :: xs, n: int, n2: int) = x :: nelehelper(x :: xs, n - 1, n2);

(* FUNCTION NAME: nele *)
(* DESCRIPTION: repeat each element in a list n times *)
fun nele(nil, n: int) = nil
    |   nele(_, 0) = nil
    |   nele(L, n:int) = if n = 1 then L else nelehelper(L, n, n);

(*#11*)
(* FUNCTION NAME: addpa *)
(* DESCRIPTION: add corresponding elements of the two lists *)
fun addpa(nil, _) = nil
    |   addpa(_, nil) = nil
    |   addpa(x :: xs, y :: ys) = (x + y) :: addpa(xs, ys);

(*#12*)
(* FUNCTION NAME: isfacthelper *)
(* DESCRIPTION: helper function for isfact *)
fun isfacthelper(1, d:int) = true
    |   isfacthelper(n:int, d:int) = 
        let
            val continue = (n mod d = 0)
            val quotient = n div d
        in
            if continue then isfacthelper(quotient, d + 1) else false
        end;
        
(* FUNCTION NAME: isfact *)
(* DESCRIPTION: determine if value is a factorial number *)
fun isfact(n: int) =  isfacthelper(n, 2);

(*#13*)
(* FUNCTION NAME: rataddhelper *)
(* DESCRIPTION: helper function for ratadd *)
fun rataddhelper(1, d:int, c:int) = (1, d)
    |   rataddhelper(n:int, 1, c:int) = (n, 1)
    |   rataddhelper(n:int, d:int, c:int) = 
        let
            val isSimplified = (c > n) orelse (c > d) 
            val isFactorable1 = (d mod n = 0)
            val isFactorable2 = (n mod c = 0) andalso (d mod c = 0) 
        in
            if isSimplified
                then (n, d) 
            else 
                if isFactorable1
                    then (1, d div n) 
                else
                    if isFactorable2
                        then rataddhelper(n div c, d div c, c)
                    else rataddhelper(n, d, c + 1)
        end;
        
(* FUNCTION NAME: ratadd *)
(* DESCRIPTION: add two rational numbers and return simplest form *)
fun ratadd((0, _): int * int, (0, _): int * int) = (0, 1)
    |   ratadd((a, b): int * int, (c, d): int * int) = 
        let
            val num = (a * d) + (b * c)
            val denom = b * d
        in
            rataddhelper((a * d) + (b * c), (b * d), 2)
        end;

(*#14*)
(* FUNCTION NAME: spliat *)
(* DESCRIPTION: split a string at a given position *)
fun spliat(st: string, 0) = [st]
    |   spliat(st: string, 1) = implode([hd(explode(st))]) :: [implode(tl(explode(st)))]
    |   spliat(st: string, n: int) = 
        implode((hd(explode(st)) :: explode(hd(spliat(implode(tl(explode(st))), n - 1))))) :: tl(spliat(implode(tl(explode(st))), n - 1));

(*#15*)
(* FUNCTION NAME: inseachhelper2 *)
(* DESCRIPTION: inseach helper function. create the list with element at location i *)
fun inseachhelper2(n, L, 0) = n :: L
    |   inseachhelper2(n, x :: xs, i: int) = x :: inseachhelper2(n, xs, i - 1);
    
(* FUNCTION NAME: inseachhelper *)
(* DESCRIPTION: inseach helper function. create the list of lists *)   
fun inseachhelper(n, L, c, nil) = [inseachhelper2(n, L, c)]
    |   inseachhelper(n, L, c, x :: xs) = inseachhelper2(n, L, c) :: inseachhelper(n, L, c + 1, xs);
    
(* FUNCTION NAME: inseach *)
(* DESCRIPTION: insert an element into each position of a list *)
fun inseach(n, nil) = [[n]]
    |   inseach(n, L) = inseachhelper(n, L, 0, L);
    
    