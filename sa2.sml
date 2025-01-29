

(* Your existing code follows here *)
(*
Name: Gijeong Lee
Time Spent:  3.5 hours
Collaborators: DeepSeek
*)

structure Unit =
struct
  fun checkExpectWith toString name test expected =
    let
      val result = test ()
    in
      if result = expected then
        print ("Test passed: " ^ name ^ "\n")
      else
        print ("Test failed: " ^ name ^ "\nExpected: " ^ toString expected ^ "\nGot: " ^ toString result ^ "\n")
    end

  fun checkExnWith toString name test exnName =
    let
      val result = (test (); "No exception raised")
        handle e => "Exception raised: " ^ exnName
    in
      if result = "Exception raised: " ^ exnName then
        print ("Test passed: " ^ name ^ "\n")
      else
        print ("Test failed: " ^ name ^ "\nExpected exception: " ^ exnName ^ "\nGot: " ^ result ^ "\n")
    end

  fun boolString b = if b then "true" else "false"
  fun intString i = Int.toString i
  fun stringString s = "\"" ^ s ^ "\""
  fun charString c = "\"" ^ str c ^ "\""
  fun listString toString xs = "[" ^ String.concatWith ", " (List.map toString xs) ^ "]"
  fun pairString toString1 toString2 (x, y) = "(" ^ toString1 x ^ ", " ^ toString2 y ^ ")"

  fun reportWhenFailures () = ()
end


(***** Problem A *****)
(* Define a function `mynull : 'a list -> bool` *)
fun mynull [] = true
  | mynull _  = false

(* Unit tests for mynull *)
val () = Unit.checkExpectWith Unit.boolString "mynull empty list" (fn () => mynull []) true
val () = Unit.checkExpectWith Unit.boolString "mynull non-empty list" (fn () => mynull [1, 2, 3]) false

(***** Problem B *****)
(* Define a function `firstVowel : char list -> bool` *)
fun firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel _         = false

(* Unit tests for firstVowel *)
val () = Unit.checkExpectWith Unit.boolString "firstVowel with vowel" (fn () => firstVowel [#"a", #"b", #"c"]) true
val () = Unit.checkExpectWith Unit.boolString "firstVowel without vowel" (fn () => firstVowel [#"b", #"c", #"d"]) false
val () = Unit.checkExpectWith Unit.boolString "firstVowel empty list" (fn () => firstVowel []) false

(***** Problem C *****)
(* Define `reverse : 'a list -> 'a list` using `foldl` *)
fun reverse xs = List.foldl (fn (x, acc) => x::acc) [] xs

(* Unit tests for reverse *)
val () = Unit.checkExpectWith (Unit.listString Unit.intString) "reverse list" (fn () => reverse [1, 2, 3]) [3, 2, 1]
val () = Unit.checkExpectWith (Unit.listString Unit.stringString) "reverse empty list" (fn () => reverse []) []

(***** Problem D *****)
(* Define `minlist : int list -> int` using `foldl` *)
exception EmptyList
fun minlist [] = raise EmptyList
  | minlist (x::xs) = List.foldl Int.min x xs

(* Unit tests for minlist *)
val () = Unit.checkExpectWith Unit.intString "minlist non-empty" (fn () => minlist [3, 1, 4, 1, 5, 9]) 1
val () = Unit.checkExnWith Unit.intString "minlist empty" (fn () => minlist []) "EmptyList"

(***** Problem E *****)
(* Define `zip: 'a list * 'b list -> ('a * 'b) list` *)
exception Mismatch
fun zip ([], []) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
  | zip _ = raise Mismatch

(* Unit tests for zip *)
val () = Unit.checkExpectWith (Unit.listString (Unit.pairString Unit.intString Unit.stringString)) "zip lists" (fn () => zip ([1, 2, 3], ["a", "b", "c"])) [(1, "a"), (2, "b"), (3, "c")]
val () = Unit.checkExnWith (Unit.listString (Unit.pairString Unit.intString Unit.stringString)) "zip mismatched lists" (fn () => zip ([1, 2], ["a"])) "Mismatch"

(***** Problem F *****)
(* Define `concat : 'a list list -> 'a list` *)
fun concat xss = List.foldr (fn (xs, acc) => xs @ acc) [] xss

(* Unit tests for concat *)
val () = Unit.checkExpectWith (Unit.listString Unit.intString) "concat lists" (fn () => concat [[1], [2, 3, 4], [], [5, 6]]) [1, 2, 3, 4, 5, 6]
val () = Unit.checkExpectWith (Unit.listString Unit.stringString) "concat empty list" (fn () => concat []) []

(***** Problem G *****)
(* Define `isDigit : char -> bool` *)
fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _    = false

(* Unit tests for isDigit *)
val () = Unit.checkExpectWith Unit.boolString "isDigit true" (fn () => isDigit #"5") true
val () = Unit.checkExpectWith Unit.boolString "isDigit false" (fn () => isDigit #"a") false

(***** Problem H *****)
(* Define `isAlpha : char -> bool` *)
fun isAlpha c =
  let
    val ord = Char.ord c
  in
    (ord >= 65 andalso ord <= 90) orelse (ord >= 97 andalso ord <= 122)
  end

(* Unit tests for isAlpha *)
val () = Unit.checkExpectWith Unit.boolString "isAlpha true" (fn () => isAlpha #"a") true
val () = Unit.checkExpectWith Unit.boolString "isAlpha false" (fn () => isAlpha #"5") false

(***** Problem I *****)
(* Define `svgCircle : int * int * int * string -> string` *)
fun svgCircle (cx, cy, r, fill) =
  "<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" ^ Int.toString cy ^ "\" r=\"" ^ Int.toString r ^ "\" fill=\"" ^ fill ^ "\" />"

(* Unit tests for svgCircle *)
val () = Unit.checkExpectWith Unit.stringString "svgCircle example 1" (fn () => svgCircle (120, 150, 60, "white")) "<circle cx=\"120\" cy=\"150\" r=\"60\" fill=\"white\" />"
val () = Unit.checkExpectWith Unit.stringString "svgCircle example 2" (fn () => svgCircle (200, 300, 100, "red")) "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"

(***** Problem J *****)
(* Define `partition : ('a -> bool) -> 'a list -> 'a list * 'a list` *)
fun partition _ [] = ([], [])
  | partition p (x::xs) =
      let
        val (trueList, falseList) = partition p xs
      in
        if p x then (x::trueList, falseList)
        else (trueList, x::falseList)
      end

(* Unit tests for partition *)
val () = Unit.checkExpectWith (Unit.pairString (Unit.listString Unit.intString) (Unit.listString Unit.intString)) "partition even/odd" (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5]) ([2, 4], [1, 3, 5])
val () = Unit.checkExpectWith (Unit.pairString (Unit.listString Unit.charString) (Unit.listString Unit.charString)) "partition alpha/non-alpha" (fn () => partition Char.isAlpha [#"a", #"1", #"b", #"2", #"c"]) ([#"a", #"b", #"c"], [#"1", #"2"])

(* Report all unit test failures *)
val () = Unit.reportWhenFailures ()