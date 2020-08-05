{
(*
  Usage: ./compte.native file1.txt file2.txt number_of_more_present_words_to_display
*)
}

(* Wow.Ugly. Well, it does the job. *)
let alpha = ['a' - 'z' 'A' - 'Z'] | "é" | "è" | "ù" | "à" | "ê" | "â" | "û" | "î" | "ç" | "À"

rule word acc = parse
  | eof { acc }
  | alpha + as w { word (Hamt.String.modify_def 0 w succ acc) lexbuf}
  | _ { word acc lexbuf }

{
let rec inser k v = function
  | [] -> [k, v]
  | (k', v') :: tl as li ->
      if v <= v' then (k, v) :: li
      else (k', v') :: (inser k v tl)

let inser_if k v li =
  if v > snd (List.hd li)
  then inser k v (List.tl li) else li

let rec string_of_list = function
  | [] -> ""
  | (k, v) :: tl -> Printf.sprintf "%s [%d]\n%s" k v (string_of_list tl)

let table1 = word Hamt.String.empty (Lexing.from_channel (open_in Sys.argv.(1)))
let table2 = word Hamt.String.empty (Lexing.from_channel (open_in Sys.argv.(2)))

let rec list_make n x =
  match n with
  | 0 -> []
  | n -> x :: (list_make (n - 1) x)

let () =
  let n_max = int_of_string (Sys.argv.(3)) in
  let f table =
    Printf.printf "
Nombre total de mots : %d
Nombre de mots uniques : %d
%d mots les plus présents : \n%s"
      (Hamt.String.fold (fun _key -> (+)) table 0)
      (Hamt.String.cardinal table)
      n_max
      (string_of_list
         (Hamt.String.fold inser_if table (list_make n_max ("", 0))))
  in
  f table1;
  f table2;
  f (Hamt.String.intersect (+) table1 table2)
}
