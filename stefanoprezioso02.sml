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







    
    