(* 
    Stefano Prezioso
    COSC 341 Assignment 1
    if-then-else style
*)

(*#1*)
(* FUNCTION NAME: dispnthc *)
(* DESCRIPTION: display the nth character of a String *)
fun dispnthc(myString:string, n:int) =
    if n <= 0 then chr(0) else
        if n = 1 then hd(explode(myString)) else dispnthc(implode(tl(explode(myString))), n - 1);

(*#2*)
(* FUNCTION NAME: delnthc *)
(* DESCRIPTION: delete the nth character of a String *)
fun delnthc(myString:string, n:int) = 
    if n <= 0 then myString else
        if n = 1 then implode(tl(explode(myString))) else 
            str(hd(explode(myString))) ^ delnthc(implode(tl(explode(myString))), n - 1);

(*#3*)
(* FUNCTION NAME: multinhelper *)
(* DESCRIPTION: helper function for multin *)
fun multinhelper(value, multiplier, numOfTimes, counter) = 
    if counter = 0 then value :: multinhelper(value, multiplier, numOfTimes, counter + 1) else 
        if counter = numOfTimes then [value * multiplier] else 
            (value * multiplier) :: multinhelper(value * multiplier, multiplier, numOfTimes, counter + 1);

(* FUNCTION NAME: multin *)
(* DESCRIPTION: take a list of three and create a list of elements a * b c times *)
fun multin([a, b, c]) = 
    if c = 0 then [a] else multinhelper(a, b, c, 0);

(*#4*)
(* FUNCTION NAME: remv *)
(* DESCRIPTION: remove instances of n in the list L *)
fun remv(n, L) = 
    if null(L) then nil else
        if null(tl(L)) andalso hd(L) = n then nil else
            if null(tl(L)) andalso hd(L) <> n then L else
                if hd(L) = n then remv(n, tl(L)) else hd(L) :: remv(n, tl(L));

(*#5*)
(* FUNCTION NAME: remvdub *)
(* DESCRIPTION: remove any repeated instances in the list L *)
fun remvdub(L) = 
    if null(L) then nil else
        if null(tl(L)) then L else
            hd(L) :: remvdub(remv(hd(L), tl(L)));

(*#6*)
(* FUNCTION NAME: int2str *)
(* DESCRIPTION: convert an integer to a numeric String *)
fun int2str(n:int) = 
    if n div 10 = 0 then str(chr((n mod 10) + 48)) 
        else int2str(n div 10) ^ str(chr((n mod 10) + 48));

(*#7*)
(* FUNCTION NAME: str2inthelper *)
(* DESCRIPTION: helper function for str2int *)
fun str2inthelper(n:int, L: char list) = if null(L) then 0 else 
    if null(tl(L)) then (n * 10) + (ord(hd(L)) - 48) else
        str2inthelper((n * 10) + (ord(hd(L)) - 48), tl(L));
        
(* FUNCTION NAME: str2int *)
(* DESCRIPTION: convert a numeric String to an int *)
fun str2int(n:string) = 
    if null(tl(explode(n))) then (ord(hd(explode(n))) - 48) 
        else str2inthelper(ord(hd(explode(n))) - 48, tl(explode(n)));

(*#8*)
(* FUNCTION NAME: indehelper *)
(* DESCRIPTION: helper function for inde *)
fun indehelper(n, L, counter: int) = if null(L) then nil else if n = hd(L) 
    then counter :: indehelper(n, tl(L), counter + 1)
        else indehelper(n, tl(L), counter + 1);
        
(* FUNCTION NAME: inde *)
(* DESCRIPTION: returns index of occurence of given value starting at 1 *)
fun inde(n, L) = if null(L) then nil else
    indehelper(n, L, 1);

(*#9*)
(* FUNCTION NAME: occrhelper *)
(* DESCRIPTION: helper function for occr *)
fun occrhelper(n, L, counter: int) = if null(L) then (n, counter) else
    if n = hd(L) then occrhelper(n, tl(L), counter + 1) else
        occrhelper(n, tl(L), counter);
        
(* FUNCTION NAME: occr *)
(* DESCRIPTION: dislay the occruence of an element in a list *)
fun occr(L) = if null(L) then nil else
    occrhelper(hd(L), tl(L), 1) :: occr(remv(hd(L), L));

(*#10*)
(* FUNCTION NAME: nelehelper *)
(* DESCRIPTION: helper function for nele *)
fun nelehelper(L, n: int, n2: int) = if null(L) then nil else 
        if n = 0 then nelehelper(tl(L), n2, n2) else
            hd(L) :: nelehelper(L, n - 1, n2);

(* FUNCTION NAME: nele *)
(* DESCRIPTION: repeat each element in a list n times *)
fun nele(L, n: int) = if null(L) orelse n = 0 then nil else
    if n = 1 then L else nelehelper(L, n, n);

(*#11*)
(* FUNCTION NAME: addpa *)
(* DESCRIPTION: add corresponding elements of the two lists *)
fun addpa(L, M) = if null(L) orelse null(M) then nil else
    (hd(L) + hd(M)) :: addpa(tl(L), tl(M));

(*#12*)
(* FUNCTION NAME: isfacthelper *)
(* DESCRIPTION: helper function for isfact *)
fun isfacthelper(n:int, d:int) = if n = 1 then true else 
        if n mod d = 0 then isfacthelper(n div d, d + 1) else false;
        
(* FUNCTION NAME: isfact *)
(* DESCRIPTION: determine if value is a factorial number *)
fun isfact(n: int) = isfacthelper(n, 2);

(*#13*)
(* FUNCTION NAME: rataddhelper *)
(* DESCRIPTION: helper function for ratadd *)
fun rataddhelper(n:int, d:int, c:int) = if (n = 1) orelse (d = 1) orelse (c > n) orelse (c > d) 
    then (n, d) else if (d mod n = 0) then (1, d div n) else
            if (n mod c = 0) andalso (d mod c = 0) then rataddhelper(n div c, d div c, c)
                else rataddhelper(n, d, c + 1);
        
(* FUNCTION NAME: ratadd *)
(* DESCRIPTION: add two rational numbers and return simplest form *)
fun ratadd((a, b): int * int, (c, d): int * int) = 
    if (a = 0) orelse (c = 0) then (0, 1) else 
        rataddhelper((a * d) + (b * c), (b * d), 2);

(*#14*)
(* FUNCTION NAME: spliat *)
(* DESCRIPTION: split a string at a given position *)
fun spliat(st: string, n: int) = 
    if n = 0 
        then [st]
    else if n = 1 
        then implode([hd(explode(st))]) :: [implode(tl(explode(st)))]
    else
        implode((hd(explode(st)) :: explode(hd(spliat(implode(tl(explode(st))), n - 1))))) :: tl(spliat(implode(tl(explode(st))), n - 1));

(*#15*)
(* FUNCTION NAME: inseachhelper2 *)
(* DESCRIPTION: inseach helper function. create the list with element at location i *)
fun inseachhelper2(n, L, i: int) = if i = 0 then n :: L else
    hd(L) :: inseachhelper2(n, tl(L), i - 1);
    
(* FUNCTION NAME: inseachhelper *)
(* DESCRIPTION: inseach helper function. create the list of lists *)    
fun inseachhelper(n, L, c, M) = if null(M) then inseachhelper2(n, L, c) :: nil else
    inseachhelper2(n, L, c) :: inseachhelper(n, L, c + 1, tl(M));
    
(* FUNCTION NAME: inseach *)
(* DESCRIPTION: insert an element into each position of a list *)
fun inseach(n, L) = if null(L) then [[n]] else
    inseachhelper(n, L, 0, L);

