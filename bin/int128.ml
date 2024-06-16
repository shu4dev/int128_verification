(* every int128 has 16 bytes *)
type int128 = Int128 of (int * int * int * int * int * int * int * int *
                         int * int * int * int * int * int * int * int)

(* useful constants *)
let zero128 =         Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) 
let one128 =          Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1) 
let maxint128 =       Int128 (255, 255, 255, 255, 255, 255, 255, 255,
                              255, 255, 255, 255, 255, 255, 255, 255) 

(* these two functions are intended for internal use only *)
let int128_to_list i = match i with
    | Int128 ( i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8,
               i9, i10, i11, i12, i13, i14, i15, i16) ->
      [i1; i2; i3; i4; i5; i6; i7; i8; i9; i10; i11; i12; i13; i14; i15; i16]
let int128_from_list lst =
    let rec check_bytes lst = match lst with
        | [] -> ()
        | first :: rest ->
           if first < 0 || first > 255 then failwith "not a list of bytes"
           else check_bytes rest in
    (check_bytes lst;
     match lst with
     | [ i1;  i2;  i3;  i4;  i5;  i6;  i7;  i8;
         i9; i10; i11; i12; i13; i14; i15; i16] ->
       Int128 ( i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8,
                i9, i10, i11, i12, i13, i14, i15, i16)
    | _ -> failwith "not a list of 16 bytes")

let to_hex n = match n with
    | 0  -> "0"
    | 1  -> "1"
    | 2  -> "2"
    | 3  -> "3"
    | 4  -> "4"
    | 5  -> "5"
    | 6  -> "6"
    | 7  -> "7"
    | 8  -> "8"
    | 9  -> "9"
    | 10 -> "a"
    | 11 -> "b"
    | 12 -> "c"
    | 13 -> "d"
    | 14 -> "e"
    | 15 -> "f"
    | _  -> "x"

let to_string128 i = match i with
     | Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, i1, i2, i3, i4) ->
         Int.to_string (i1 * 16777216 + i2 * 65536 + i3 * 256 + i4)
     | Int128 (255, 255, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 255, 255, 255, 255, 255) -> "maxint128"
     | _ -> "large int"

let to_hex128 i = 
     let rec hex_string = function
         | [] -> ""
         | byte :: rest ->
            to_hex (byte / 16) ^ (to_hex (byte mod 16)) ^ hex_string rest in
     let rec remove_leading_zeros lst = match lst with
         | [] -> "00"
         | 0 :: rest -> remove_leading_zeros rest
         | _ -> hex_string lst in
     if i = maxint128 then "max"
     else remove_leading_zeros (int128_to_list i)

(* this version only works for numbers < 2^32, but modern implementations
 * of ocaml use 64-bit ints, so it would be possible to go up to 2^63-1. *)
let from_i128 i128 = match i128 with
     | Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, i1, i2, i3, i4) ->
         i1 * 16777216 + i2 * 65536 + i3 * 256 + i4;
     | _ -> (print_endline ("conversion failure " ^ to_string128 i128);
         failwith "conversion failure")

(* assert that every element of the result is between 0 and 255, inclusive *)
let to_i128 i =
     Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             i / 16777216 mod 256,
             i / 65536 mod 256,
             i / 256 mod 256,
             i       mod 256)

let add_lists a b =
     let rec extend a b =
          if List.length a = List.length b      then (a, b)
          else if List.length a < List.length b then extend (0 :: a) b
          else                                       extend a (0 :: b) in
     let (ea, eb) = extend a b in
     let rec add a b =
          match (a, b) with
          | ([], []) -> (0, [])
          | (a_digit :: a_rest, b_digit :: b_rest) ->
            let (rest_carry, rest_sum) = add a_rest b_rest in
            let digit_sum = a_digit + b_digit + rest_carry in
            (digit_sum / 256, digit_sum mod 256 :: rest_sum)
          | _ -> failwith ("inconsistency in add") in
     match add ea eb with
     | (0, result) -> result
     | (carry, result) -> carry :: result

(* assert that overflow will fail with "not a list of 16 bytes"
   (for example, add128 maxint128 maxint128 will overflow)
   assert that if there is no overflow, the result is a + b *)
let add128 a b =
     int128_from_list (add_lists (int128_to_list a) (int128_to_list b))
let add128mod a b =
     let sum = add_lists (int128_to_list a) (int128_to_list b) in
     let rec shorten lst = match lst with
         | [] -> failwith "illegal list length 0"
         | first :: rest ->
            if List.length lst > 16 then shorten rest else lst in
     int128_from_list (shorten sum)
let inc128 a = add128 a one128

(* simple multiply by adding, slow when b is large
let m a b =
     let rec m a b counter product =
          if counter = b then product
          else m a b (add128 one128 counter) (add128 a product) in
     int128_from_list (m (int128_to_list a) (int128_to_list b) zero128 zero128)
*)

(* assert that if there is no overflow, the result is a * b *)
let mul128 a b =              (* proper multiply *)
     let rec mul_digit a b =  (* assert b < 256 *)
          match a with
          | [] -> (0, [])
          | digit :: rest ->
            let (rest_carry, rest_product) = mul_digit rest b in
            let digit_product = digit * b + rest_carry in
            (digit_product / 256, digit_product mod 256 :: rest_product) in
     let rec shift a = function  (* function matches an implied parameter *)
          | [] -> a
          | _ :: rest -> shift (a @ [0]) rest in
     let rec remove_zeros n lst = match (n, lst) with
              | (0, _) -> lst
              | (n, 0 :: rest) -> remove_zeros (n - 1) rest
              | _ -> failwith ("multiplication overflow") in
     let rec mul a b =
          match b with
          | [] -> (0, [])
          | digit :: rest ->
            let (digit_carry, unshifted_digit_product) = mul_digit a digit in
            let digit_product = shift unshifted_digit_product rest in
            let (rest_carry, rest_product) = mul a rest in
            (digit_carry + rest_carry, add_lists digit_product rest_product)
     in match mul (int128_to_list a) (int128_to_list b) with
        | (0, result) ->
            int128_from_list (remove_zeros (List.length result - 16) result)
        | _ -> failwith "multiplication overflow (carry)"

let compare128 a b = match (a, b) with
     (Int128 (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9,
              a10, a11, a12, a13, a14, a15),
      Int128 (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9,
              b10, b11, b12, b13, b14, b15)) ->
     if a = b then 0
     else if a0 < b0 then -1 else if a0 > b0 then 1  (* else a0 = b0 *)
     else if a1 < b1 then -1 else if a1 > b1 then 1
     else if a2 < b2 then -1 else if a2 > b2 then 1
     else if a3 < b3 then -1 else if a3 > b3 then 1
     else if a4 < b4 then -1 else if a4 > b4 then 1
     else if a5 < b5 then -1 else if a5 > b5 then 1
     else if a6 < b6 then -1 else if a6 > b6 then 1
     else if a7 < b7 then -1 else if a7 > b7 then 1
     else if a8 < b8 then -1 else if a8 > b8 then 1
     else if a9 < b9 then -1 else if a9 > b9 then 1
     else if a10 < b10 then -1 else if a10 > b10 then 1
     else if a11 < b11 then -1 else if a11 > b11 then 1
     else if a12 < b12 then -1 else if a12 > b12 then 1
     else if a13 < b13 then -1 else if a13 > b13 then 1
     else if a14 < b14 then -1 else if a14 > b14 then 1
     else if a15 < b15 then -1 else if a15 > b15 then 1
     else assert false   (* we checked at the top for a = b *)

let sub128 a b = (* a - b *)
     if b = zero128 then a    (* negate doesn't work if b is zero *)
     else if compare128 a b < 0 then
           failwith "negative subtraction result not supported"
     else
      let list_b = int128_to_list b in
      let rec negate lst = match lst with  (* pre: lst is not zero *)
          | [] -> []
          | [last] -> [256 - last]
          | first :: rest -> (255 - first) :: negate rest in
      match add_lists (int128_to_list a) (negate list_b) with
      | [] -> failwith "illegal subtraction result []"
      | first :: rest ->
         (assert (List.length rest = 16);   (* addition must overflow! *)
          int128_from_list rest)
let dec128 a = sub128 a one128

let test128 () =
     let n2 = add128 one128 one128 in
     let n255 = Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255) in
     let n256 = Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0) in
     let n64k = Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0) in
     let n4g  = Int128 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0) in
     let p1   = mul128 n64k n255 in
     let p2   = mul128 n64k n256 in
     let p3   = mul128 n4g n4g   in
     let tst1 = Int128 (0, 0, 0, 0, 0, 0, 0, 0,  (* 123456789, hex 75BCD15 *)
                        0, 0, 0, 0, 7, 91, 205, 21) in
     let tst2 = Int128 (0, 0, 0, 0, 0, 0, 0, 0,  (* 4321, hex 10E1 *)
                        0, 0, 0, 0, 0, 0, 16, 225) in
     let res  = Int128 (0, 0, 0, 0, 0, 0, 0, 0,  (* 533456785269, x7C34808F75 *)
                        0, 0, 0, 124, 52, 128, 143, 117) in
     let r12  = mul128 tst1 tst2 in
     let r21  = mul128 tst2 tst1 in
     let _ = assert (res = r12) in
     let _ = assert (res = r21) in
     print_endline ("two is " ^ (to_string128 n2)   ^ "/" ^ (to_hex128 n2));
     print_endline ("255 is " ^ (to_string128 n255) ^ "/" ^ (to_hex128 n255));
     print_endline ("256 is " ^ (to_string128 n256) ^ "/" ^ (to_hex128 n256));
     print_endline ("64k is " ^ (to_string128 n64k) ^ "/" ^ (to_hex128 n64k));
     print_endline ("4g is "  ^ (to_string128 n4g)  ^ "/" ^ (to_hex128 n4g));
     print_endline ("64k*255 is "  ^ (to_string128 p1)  ^ "/" ^ (to_hex128 p1));
     print_endline ("64k*256 is "  ^ (to_string128 p2)  ^ "/" ^ (to_hex128 p2));
     print_endline ("4g*4g is "  ^ (to_string128 p3)  ^ "/" ^ (to_hex128 p3));
     print_endline ((to_string128 tst1) ^ "/" ^ (to_hex128 tst1) ^ " * " ^
                    (to_string128 tst2) ^ "/" ^ (to_hex128 tst2) ^ " = " ^
                                                (to_hex128 r12)  ^ " = " ^
                                                (to_hex128 r21)  ^ " = " ^
                                                (to_hex128 res));
     print_endline ((to_hex128 res) ^ " * " ^ (to_hex128 res) ^ " = " ^
                    (to_hex128 (mul128 res res)) ^ " =? 3c42e74f742f4d63eb79");
     print_endline ("maxint is " ^ (to_hex128 maxint128))