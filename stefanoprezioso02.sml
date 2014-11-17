(* Problem 1 *)
(* FUNCTION NAME: listChopper *)
(* DESCRIPTION: takes a list and a delimiter and chops the list at the first *)
(*              instance of the delimiter. returns the two pieces without del *)
fun listChopper(nil, _) = (nil, nil)
|   listChopper(x::nil:char list, del:char) = if x = del then (nil, nil) else ([x], nil)
|   listChopper(x::xs:char list, del:char) = if x = del 
        then (nil, xs)
        else (x :: #1(listChopper(xs, del)),
            #2(listChopper(xs, del)));

(* FUNCTION NAME: strTokHelper *)
(* DESCRIPTION: takes a char list and delimiter and assembles a list of strings *)   
(*              from the listChopper *)
fun strTokHelper(L, del) = if null(L) then nil
    else if null(#2(listChopper(L, del)))
    then [implode(#1(listChopper(L, del)))]
    else implode(#1(listChopper(L, del))) :: strTokHelper(#2(listChopper(L, del)), del);
        

(* FUNCTION NAME: stringTokenizer *)
(* DESCRIPTION: takes a string and a delimiter and tokenizes the string *)
fun stringTokenizer(myString:string, delimiter:char) = 
    strTokHelper(explode(myString), delimiter);

(* Problem 2 *)
(* FUNCTION NAME: subStringHelper *)
(* DESCRIPTION: All parameters except index are char list.
subStringHelper(workingSub, workingMain, initialMain, initialSub, index) *)
fun subStringHelper(nil, _, _, _, index:int) = index
|   subStringHelper(_, nil, _, _, _) = ~1
|   subStringHelper(x::nil:char list, y::nil:char list, _, _, index:int) = 
        if x = y then index else ~1
|   subStringHelper(x::nil:char list, y::ys:char list, z::zs, M, index:int) = 
        if x = y 
        then index 
        else subStringHelper(M, zs, zs, M, index + 1)
|   subStringHelper(x::xs, y::nil, _, _, _) = ~1
|   subStringHelper(x::xs:char list, y::ys:char list, z::zs, R, index:int) = 
        if x = y
        then subStringHelper(xs, ys, z::zs, R, index)
        else subStringHelper(R, zs, zs, R, index + 1)
|   subStringHelper(_, _, _, _, _) = ~1;

(* FUNCTION NAME: subString *)
(* DESCRIPTION: Returns the starting index of the substring. ~1 if not *)
fun subString(subStr:string, mainStr:string) = 
    subStringHelper(explode(subStr), explode(mainStr), 
        explode(mainStr), explode(subStr), 0);

(* Problem 3 *)
(* FUNCTION NAME: factorial *)
(* DESCRIPTION: returns the factorial of n *)
fun factorial(n:int) = if n = 0 
    then 1
    else n * factorial(n - 1);

(* FUNCTION NAME: binCo *)
(* DESCRIPTION: returns the binomial coefficient value at n,k *)
fun binCo(n:int, k:int) = 
    (factorial(n)) div (factorial(k) * factorial(n - k));

(* FUNCTION NAME: pascalRow *)
(* DESCRIPTION: returns a row of the Pascal Triangle as an int list *)
fun pascalRow(row:int, col:int) = 
    if row = col
    then [1]
    else binCo(row, col) :: pascalRow(row, col + 1);

(* FUNCTION NAME: pTriangleHelper *)
(* DESCRIPTION: builds the pascal triangle int list list *)
fun pTriangleHelper(row:int, counter:int) = 
    if counter > row
    then nil
    else pascalRow(counter, 0) :: pTriangleHelper(row, counter + 1);
    
(* FUNCTION NAME: pTriangles *)
(* DESCRIPTION: Returns int lists of increasing pascal triangle rows *)
fun pTriangles(row:int) = if row = 1 then [[1]]
    else pTriangleHelper(row, 1);

(* Problem 4 *)
(* FUNCTION NAME: searchAndAdd *)
(* DESCRIPTION: increments corresponding tuple in list*)
fun searchAndAdd(x, nil) = [(x, 1)]
|   searchAndAdd(x, (a,b:int)::ls) = if x = a then (a, b + 1) :: ls
        else (a,b) :: searchAndAdd(x, ls);

(* FUNCTION NAME: occr *)
(* DESCRIPTION: takes a list and returns values with number of occurences *)
fun occr(nil, L) = L
|   occr(x::nil, L) = searchAndAdd(x, L)
|   occr(x::xs, L) = occr(xs, searchAndAdd(x, L));

(* FUNCTION NAME: getMode *)
(* DESCRIPTION: returns a list of mode tuples*)
fun getMode(nil, nil) = nil
|   getMode(nil, L) = L
|   getMode((x,y)::nil, (l,m)::nil) = if y > m then [(x,y)]
        else if y = m then (x,y)::[(l,m)] else [(l,m)]
|   getMode((x,y)::nil, (l,m)::ls) = if y > m then [(x,y)]
        else if y = m then (x,y)::(l,m)::ls else (l,m)::ls
|   getMode((x,y)::xs, (l,m)::nil) = if y > m then getMode(xs, [(x,y)])
        else if y = m then getMode(xs, (x,y)::[(l,m)]) 
        else getMode(xs, [(l,m)])
|   getMode((x,y)::xs, (l,m)::ls) = if y > m then getMode(xs, [(x,y)])
        else if y = m then getMode(xs, (x,y)::(l,m)::ls) 
        else getMode(xs, (l,m)::ls)
|   getMode(x::xs, nil) = getMode(xs, [x]);

(* FUNCTION NAME: modeL *)
(* DESCRIPTION: takes a list and returns the mode with number of occurences *)
fun modeL(L) = getMode(occr(L, nil), nil);

(* Problem 5 *)
(* FUNCTION NAME: interleave *)
(* DESCRIPTION: *)
fun interleave(x:int, nil) = [[x]]
|   interleave(x:int, l::nil:int list) = (x :: [l]) :: [l :: hd(interleave(x,nil))]
|   interleave(x:int, l::ls) = (x :: l :: ls) :: (l :: hd(interleave(x, ls))) :: interleave(x, ls) ;

(* NOT DONE *)
interleave(4, [1,2,3]);

(* FUNCTION NAME: permuHelper *)
(* DESCRIPTION: *)


(* FUNCTION NAME: permu *)
(* DESCRIPTION: generate permutation of identity list from 1 to n*)



(* Problem 6 *)
(* FUNCTION NAME: getMin *)
(* DESCRIPTION: returns minimum value from list with remaining list*)
fun getMin(nil) = (0, nil)
|   getMin(x::nil) = (x, nil)
|   getMin(x::xs) = if x <= #1(getMin(xs)) then (x, xs) 
        else (#1(getMin(xs)), x :: (#2(getMin(xs))));


(* FUNCTION NAME: sshelper *)
(* DESCRIPTION: sorts list using selection sort*)
fun sshelper(nil, nil) = (nil, nil)
|   sshelper(nil, y::nil) = ([y], nil)
|   sshelper(nil, L) = sshelper([#1(getMin(L))], #2(getMin(L)))
|   sshelper(x::nil, nil) = ([x], nil)
|   sshelper(x::nil, y::nil) = if x <= y then (x :: [y], nil) else (y :: [x], nil)
|   sshelper(x::nil, L) = sshelper(x :: [#1(getMin(L))], #2(getMin(L)))
|   sshelper(X, nil) = (X, nil)
|   sshelper(X, y::nil) = (X @ [y], nil)
|   sshelper(X, L) = sshelper(X @ [#1(getMin(L))], #2(getMin(L)));

(* FUNCTION NAME: ssort *)
(* DESCRIPTION: returns sorted list using selection sort*)
fun ssort(L:int list) = #1(sshelper(nil, L));

ssort([3,2,6,9,1,4,0,8,5,7]);


(* SEVEN IS WORKING BUT NEEDS HIGHER ORDER FUNCTION!*)
(* Problem 7 *)
(* FUNCTION NAME: isPrimeHelper *)
(* DESCRIPTION: determines whether a number is prime or not*)
fun isPrimeHelper(n:int, c:int) = if n = c then true 
    else if n mod c = 0 then false
    else isPrimeHelper(n, c + 1);

(* FUNCTION NAME: isPrime *)
(* DESCRIPTION: returns whether a number is prime or not*)
fun isPrime(n:int) = isPrimeHelper(n, 2);

(* FUNCTION NAME: plistHelper *)
(* DESCRIPTION: creates list of all prime numbers up to n*)
fun plistHelper(n:int, c:int, L) = if c < 2 then L
    else if isPrime(c) then plistHelper(n, c - 1, c :: L)
    else plistHelper(n, c - 1, L);

(* FUNCTION NAME: plist *)
(* DESCRIPTION: returns list all prime numbers up to n*)
fun plist(n:int) = plistHelper(n, n, nil);


    
    