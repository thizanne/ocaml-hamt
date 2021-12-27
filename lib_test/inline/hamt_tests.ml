module Int_map = Hamt.Int

let print pp map =
  let bindings = Int_map.bindings map in
  let pp_sep fmt () = Format.pp_print_char fmt ';' in
  let pair fmt (k, v) = Format.fprintf fmt "%d = %a" k pp v in
  Format.printf "[%a]@." (Format.pp_print_list ~pp_sep pair) bindings

let print_str map = print (fun fmt s -> Format.fprintf fmt "%S" s) map

let%expect_test "empty is empty" =
  let m = Int_map.empty in
  assert (Int_map.length m = 0);
  print_str m;
  [%expect {| [] |}]

let%expect_test "singleton" =
  let m = Int_map.singleton 2 "foo" in
  assert (Int_map.length m = 1);
  print_str m;
  [%expect {| [2 = "foo"] |}]

let%expect_test "of_list" =
  let map = Int_map.Import.AssocList.from [ (1, "foo"); (2, "bar") ] in
  print_str map;
  [%expect {| [2 = "bar";1 = "foo"] |}]

let%expect_test "of_list" =
  let map = Int_map.Import.AssocList.from [ (1, "one"); (3, "three") ] in
  let map = Int_map.map String.length map in
  print Format.pp_print_int map;
  [%expect {| [3 = 5;1 = 3] |}]

let%expect_test "remove" =
  let map =
    Int_map.Import.AssocList.from [ (1, "one"); (3, "three"); (4, "four") ]
  in
  print_str map;
  [%expect {| [3 = "three";4 = "four";1 = "one"] |}];
  let map = Int_map.remove 1 map in
  print_str map;
  [%expect {| [3 = "three";4 = "four"] |}];
  let map = Int_map.remove 10 map in
  print_str map;
  [%expect {| [3 = "three";4 = "four"] |}]

let map =
  Int_map.Import.AssocList.from [ (1, "one"); (3, "three"); (4, "four") ]

let%expect_test "map" =
  let map = Int_map.map String.length map in
  print Format.pp_print_int map;
  [%expect {| [3 = 5;4 = 4;1 = 3] |}]

let%expect_test "filter" =
  let map = Int_map.filter (fun _ x -> String.length x > 3) map in
  print_str map;
  [%expect {| [3 = "three";4 = "four"] |}]

let%expect_test "intersect - Not_found" =
  (try
     ignore (Int_map.intersect (fun _ _ -> raise Not_found) map map);
     print_endline "[FAIL] swallowed Not_found"
   with Not_found -> print_endline "[PASS] caught Not_found");
  [%expect {| [FAIL] swallowed Not_found |}]
