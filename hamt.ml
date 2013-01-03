open BitUtils

let hash x = Hashtbl.hash x

type ('k, 'v) t =
  | Empty
  | Leaf of int * 'k * 'v
  | HashCollision of int * ('k * 'v) list
  | BitmapIndexedNode of int * ('k, 'v) t array
  | ArrayNode of int * ('k, 'v) t array

let empty = Empty

let leaf h k v = Leaf (h, k, v)

let node_children = function
  | BitmapIndexedNode (_, base) -> base
  | ArrayNode (_, children) -> children
  | _ -> failwith "children"

let singleton k v = Leaf (hash k, k, v)

let is_empty x = x = Empty

let rec cardinal = function
  | Empty -> 0
  | Leaf (_, _, _) -> 1
  | HashCollision (_, pairs) -> List.length pairs
  | BitmapIndexedNode (_, base) ->
      Array.fold_left (fun acc child -> acc + cardinal child) 0 base
  | ArrayNode (_, children) ->
      Array.fold_left (fun acc child -> acc + cardinal child) 0 children

let is_tip_node = function
  | Empty | Leaf (_, _, _) | HashCollision (_, _) -> true
  | _ -> false

let node_hash = function
  | Leaf (h, _, _) -> h
  | HashCollision (h, _) -> h
  | _ -> failwith "node_hash"

(* Number of bits taken from the hashed key at every step *)
let shift_step = 5

(* Branching factor of the structure *)
let chunk = 1 lsl shift_step

(* Mask of shift_step `1` bits *)
let mask = pred chunk

(* Maximum size of a BitmapIndexedNode, arbitrary *)
let bmnode_max = chunk / 2

(* Minimum size of an ArrayNode, arbitrary (lesser than bmnode_max) *)
let arrnode_min = bmnode_max / 2

let hash_fragment shift h = (h asr shift) land mask

let option default f = function
  | None -> default
  | Some x -> f x

let remove tab ix =
  let tab' = Array.make (Array.length tab - 1) Empty in
  Array.blit tab 0 tab' 0 ix;
  Array.blit tab (succ ix) tab' ix (Array.length tab - ix - 1);
  tab'

let add_tab tab ix v =
  let tab' = Array.make (Array.length tab + 1) Empty in
  Array.blit tab 0 tab' 0 ix;
  Array.blit tab ix tab' (succ ix) (Array.length tab - ix);
  tab'.(ix) <- v;
  tab'

let set_tab tab ix v =
  let tab' = Array.copy tab in
  tab'.(ix) <- v;
  tab'

let rec combine_tip shift node1 node2 =
  match (node1, node2) with
    | Leaf (h1, k1, v1), Leaf (h2, k2, v2) when h1 = h2 ->
        HashCollision (h1, [k1, v1; k2, v2])
    | Leaf (h1, _, _), Leaf (h2, _, _) | Leaf (h1, _, _), HashCollision (h2, _) ->
        let sub_h1 = hash_fragment shift h1 in
        let sub_h2 = hash_fragment shift h2 in
        let (nodeA, nodeB) =
          if sub_h1 < sub_h2 then (node1, node2) else (node2, node1) in
        let bitmap = (1 lsl sub_h1) lor (1 lsl sub_h2) in
        BitmapIndexedNode (
          bitmap,
          if sub_h1 = sub_h2
          then [|combine_tip (shift + shift_step) node1 node2|]
          else [|nodeA; nodeB|]
        )
    | HashCollision (_, _), Leaf (_, _, _) -> combine_tip shift node2 node1
    | _ -> failwith "combine_tip"

let rec update_list update k = function
  | [] -> option [] (fun v -> [k, v]) (update None)
  | (kx, vx) as x :: xs ->
      if kx = k
      then option xs (fun v -> (k, v) :: xs) (update (Some vx))
      else x :: update_list update k xs

let expand_bitmap_node sub_hash node bitmap sub_nodes =
  let tab = Array.make chunk Empty in
  let rec fill ix jx bitmap =
    if ix = chunk then jx
    else if bitmap land 1 = 0 then fill (succ ix) jx (bitmap asr 1)
    else begin
      tab.(ix) <- sub_nodes.(jx);
      fill (succ ix) (succ jx) (bitmap asr 1)
    end in
  let n = fill 0 0 bitmap in
  tab.(sub_hash) <- node;
  ArrayNode (n, tab)

let pack_array_node to_remove nb_children children =
  let base = Array.make nb_children Empty in
  let rec loop ix jx bitmap =
    if ix = chunk then BitmapIndexedNode (bitmap, base)
    else if
        children.(ix) = Empty || to_remove ix
    then loop (succ ix) jx bitmap
    else begin
      base.(jx) <- children.(ix);
      loop (succ ix) (succ jx) (bitmap lor (1 lsl ix))
    end
  in loop 0 0 0

let rec reify_node = function
  | Empty -> Empty
  | Leaf (h, k, v) -> Leaf (h, k, v)
  | HashCollision (_, []) -> Empty
  | HashCollision (h, [k, v]) -> Leaf (h, k, v)
  | HashCollision (h, li) -> HashCollision (h, li)
  | BitmapIndexedNode (bitmap, base) ->
      if Array.length base = 0 then Empty
      else if Array.length base = 1 && is_tip_node base.(0) then base.(0)
      else if Array.length base > bmnode_max then failwith "reify_node"
      else BitmapIndexedNode (bitmap, base)
  | ArrayNode (nb_children, children) ->
      if nb_children < arrnode_min
      then reify_node (pack_array_node (fun _ -> false) nb_children children)
      else ArrayNode (nb_children, children)

let bitmap_to_array bitmap base =
  let children = Array.make chunk Empty
  and n = ref 0 in
  for i = 0 to mask do
    if nth_bit_set bitmap i
    then begin
      children.(i) <- base.(!n);
      incr n
    end
  done;
  children

type change =
  | Nil
  | Added
  | Modified
  | Removed

let change old_is_empty new_is_empty =
  if old_is_empty then
    if new_is_empty then Nil
    else Added
  else
    if new_is_empty then Removed
    else Modified

let rec alter_node ?(mute=false) shift hash key update = function
  | Empty ->
      option Empty (leaf hash key) (update None)
  | Leaf (h, k, v) as leaf1 ->
      if k = key
      then option Empty (leaf h k) (update (Some v))
      else
        option leaf1
          (fun x -> combine_tip shift leaf1 (Leaf (hash, key, x)))
          (update None)
  | HashCollision (h, pairs) as hash_collision ->
      if hash = h then
        let pairs = update_list update key pairs in begin
          match pairs with
            | [] -> failwith "alter_node" (* Should never happen *)
            | [(k, v)] -> leaf h k v
            | _ -> HashCollision (h, pairs)
        end
      else
        option hash_collision
          (fun x -> combine_tip shift (Leaf (hash, key, x)) hash_collision)
          (update None)
  | BitmapIndexedNode (bitmap, base) as bm_node ->
      let sub_hash = hash_fragment shift hash in
      let ix = from_bitmap bitmap sub_hash in
      let bit = 1 lsl sub_hash in
      let not_exists = bitmap land bit = 0 in
      let child = if not_exists then Empty else base.(ix) in
      let child = alter_node ~mute (shift + shift_step) hash key update child in
      begin
        match change not_exists (child = Empty) with
          | Nil -> bm_node
          | Modified ->
              if mute then begin base.(ix) <- child; bm_node end
              else BitmapIndexedNode (bitmap, set_tab base ix child)
          | Removed ->
              let bitmap = (bitmap land (lnot bit)) land mask in
              if bitmap = 0 then Empty
              else
                if Array.length base = 2 && is_tip_node base.(ix lxor 1)
                then base.(ix lxor 1)
                else BitmapIndexedNode (bitmap, remove base ix)
          | Added ->
              if Array.length base = bmnode_max
              then expand_bitmap_node sub_hash child bitmap base
              else BitmapIndexedNode (bitmap lor bit, add_tab base ix child)
      end
  | ArrayNode (nb_children, children) as arr_node ->
      let sub_hash = hash_fragment shift hash in
      let child = children.(sub_hash) in
      let child' = alter_node ~mute (shift + shift_step) hash key update child in
      match change (child = Empty) (child' = Empty) with
        | Nil -> arr_node
        | Added ->
            if mute then
              begin
                children.(sub_hash) <- child';
                ArrayNode (succ nb_children, children)
              end
            else ArrayNode (succ nb_children, set_tab children sub_hash child')
        | Modified ->
            if mute then begin children.(sub_hash) <- child'; arr_node end
            else ArrayNode (nb_children, set_tab children sub_hash child')
        | Removed ->
            if nb_children = arrnode_min then
              pack_array_node (( = ) sub_hash) nb_children children
            else
              if mute then
                begin
                  children.(sub_hash) <- Empty;
                  ArrayNode (pred nb_children, children)
                end
              else ArrayNode (pred nb_children, set_tab children sub_hash Empty)

let rec copy hamt = match hamt with
  | Empty | Leaf (_, _, _) | HashCollision (_, _) -> hamt
  | BitmapIndexedNode (bitmap, base) ->
      BitmapIndexedNode (bitmap, Array.map copy base)
  | ArrayNode (nb_children, children) ->
      ArrayNode (nb_children, Array.map copy children)

let alter_mute ?(shift=0) key update hamt =
  alter_node ~mute:true shift (hash key) key update hamt

let alter key update hamt =
  alter_node 0 (hash key) key update hamt

let add k v hamt =
  alter k (fun _ -> Some v) hamt

let remove k hamt =
  alter k (fun _ -> None) hamt

let update k f hamt =
  alter k (function | None -> None | Some v -> f v) hamt

let modify k f hamt =
  alter k (function | None -> raise Not_found | Some v -> Some (f v)) hamt

let modify_def v0 k f hamt =
  alter k (function | None -> Some (f v0) | Some v -> Some (f v)) hamt

let adjust k f hamt =
  alter k (function | None -> None | Some v -> Some (f v)) hamt

let rec alter_hc f = function
  | [] -> []
  | (k, v) :: xs ->
      begin
        match f k v with
          | None -> alter_hc f xs
          | Some w -> (k, w) :: alter_hc f xs
      end

and alter_bmnode f indices base =
  let rec aux n = function
    | [] -> [], []
    | i :: is ->
        begin
          match alter_all f base.(n) with
            | Empty -> aux (succ n) is
            | x ->
                let (iss, bss) = aux (succ n) is in
                (i :: iss, x :: bss)
        end
  in aux 0 indices

and alter_all ?(mute=false) f = function
  | Empty -> Empty
  | Leaf (h, k, v) -> option Empty (leaf h k) (f k v)
  | HashCollision (h, pairs) ->
      begin
        match alter_hc f pairs with
          | [] -> Empty
          | [(k, v)] -> Leaf (h, k, v)
          | pairs' -> HashCollision (h, pairs')
      end
  | BitmapIndexedNode (bitmap, base) ->
      begin
        match alter_bmnode f (bitmap_to_indices bitmap) base with
          | _, [] -> Empty
          | _, [x] when is_tip_node x -> x
          | indices, base ->
              BitmapIndexedNode (indices_to_bitmap indices, (Array.of_list base))
      end
  | ArrayNode (nb_children, children) ->
      let children = Array.map (alter_all f) children in
      let nb_children = Array.fold_left
        (fun n v -> if v = Empty then n else succ n) 0 children in
      if nb_children < arrnode_min
      then pack_array_node (fun _ -> false) nb_children children
      else ArrayNode (nb_children, children)

let map f hamt =
  alter_all (fun _k v -> Some (f v)) hamt

let filter f hamt =
  alter_all (fun _k v -> if f v then Some v else None) hamt

let filter_map f hamt =
  alter_all f hamt

let mapi f hamt =
  alter_all (fun k v -> Some (f k v)) hamt

let filteri f hamt =
  alter_all (fun k v -> if f k v then Some v else None) hamt

let rec iter f = function
  | Empty -> ()
  | Leaf (_, k, v) -> f k v
  | HashCollision (_, pairs) -> List.iter (fun (x, y) -> f x y) pairs
  | BitmapIndexedNode (_, base) -> Array.iter (iter f) base
  | ArrayNode (_, children) -> Array.iter (iter f) children

let find key =
  let rec find shift hash key = function
    | Empty -> raise Not_found
    | Leaf (_, k, v) -> if k = key then v else raise Not_found
    | HashCollision (_, pairs) -> List.assoc key pairs
    | BitmapIndexedNode (bitmap, base) ->
        let sub_hash = hash_fragment shift hash in
        let bit = 1 lsl sub_hash in
        if bitmap land bit = 0 then raise Not_found
        else find (shift + shift_step) hash key base.(from_bitmap bitmap sub_hash)
    | ArrayNode (_, children) ->
        let child = children.(hash_fragment shift hash) in
        if child = Empty then raise Not_found
        else find (shift + shift_step) hash key child
  in find 0 (hash key) key

let mem key hamt =
  try
    let _ = find key hamt in true
  with
    | Not_found -> false

let rec foldi f hamt v0 =
  match hamt with
    | Empty -> v0
    | Leaf (_, k, v) -> f k v v0
    | HashCollision (_, pairs) ->
        List.fold_right (fun (k, v) acc -> f k v acc) pairs v0
    | BitmapIndexedNode (_, base) ->
        Array.fold_right (foldi f) base v0
    | ArrayNode (_, children) ->
        Array.fold_right (foldi f) children v0

let fold f hamt v0 =
  foldi (fun _k v acc -> f v acc) hamt v0

let to_assoc hamt =
  foldi (fun k v acc -> (k, v) :: acc) hamt []

let bindings hamt = to_assoc hamt

let for_all f hamt =
  foldi (fun k v acc -> f k v && acc) hamt true

let exists f hamt =
  foldi (fun k v acc -> f k v || acc) hamt false

let exists_f = exists

let rec choose = function
  | Empty -> raise Not_found
  | Leaf (_, k, v) -> (k, v)
  | HashCollision (_, li) -> List.hd li
  | BitmapIndexedNode (_, base) -> choose base.(0)
  | ArrayNode (_, children) ->
      let rec loop n =
        if children.(n) = Empty then loop (succ n) else children.(n)
      in choose (loop 0)


module Import =

struct

  module type FOLDABLE = sig
    type key
    type 'v t
    val fold : (key -> 'v -> 'a -> 'a) -> 'v t -> 'a -> 'a
  end

  (*
   * TODO : use mutability when adapted
   *)
  module Make (M : FOLDABLE) =
  struct
    let add_import x hamt = M.fold add x (copy hamt)
    let import x = add_import x Empty
  end

  module List =
  struct
    let add_import assoc hamt =
      List.fold_left
        (fun acc (k, v) ->
          alter_mute k (fun _ -> Some v) acc)
        (copy hamt) assoc

    let import assoc = add_import assoc Empty
  end

  (*
  (* Requires Batteries or something like this *)

  module Map =
  struct

    let add_import x hamt =
      BatMap.foldi (fun k v acc ->
        alter_mute k (fun _ -> Some v) acc)
        x (copy hamt)

    let import x = add_import x Empty

  end
  *)

end

let array_of_rev_list li =
  let a = Array.make (List.length li) (List.hd li) in
  let rec aux n = function
    | [] -> ()
    | x :: xs -> a.(n) <- x; aux (pred n) xs
  in aux (pred (Array.length a)) li;
  a

let rec intersect_array shift f children1 children2 =
  let nb_children = ref 0 in
  for i = 0 to mask do
    let child = intersect_node shift f children1.(i) children2.(i) in
    if child <> Empty then incr nb_children;
    children1.(i) <- child
  done;
  reify_node (ArrayNode (!nb_children, children1))

and intersect_bitmap shift f li1 li2 n1 n2 base1 base2 acc =
  match (li1, li2) with
    | [], _ | _, [] -> acc
    | x :: xs, y :: ys ->
        if x < y
        then intersect_bitmap shift f xs li2 (succ n1) n2 base1 base2 acc
        else if x > y
        then intersect_bitmap shift f li1 ys n1 (succ n2) base1 base2 acc
        else
          let child =
            intersect_node shift f base1.(n1) base2.(n2) in
          intersect_bitmap shift f xs ys (succ n1) (succ n2) base1 base2
            (if child = Empty then acc else child :: acc)

and intersect_bitmap_array shift f bitmap base children =
  let nb_children = ref 0 in
  for i = 0 to mask do
    if children.(i) <> Empty then
      let bit = bitmap land (1 lsl i) in
      if bit <> 0 then
        children.(i) <- intersect_node shift f
          base.(ctpop (bitmap land pred bit)) children.(i);
      incr nb_children
  done;
  reify_node (ArrayNode (!nb_children, children))

and intersect_node shift f t1 t2 = match (t1, t2) with
  | Empty, _ -> Empty
  | Leaf (h, k, v), _ ->
      begin
        try let w = find k t2 in Leaf (h, k, f v w)
        with Not_found -> Empty
      end
  | HashCollision (h1, li1), HashCollision (h2, li2)->
      if h1 <> h2
      then Empty
      else
        begin
          match
            List.fold_left
              (fun acc (k, v) ->
                try
                  let w = List.assoc k li2 in
                  (k, f v w) :: acc
                with Not_found -> acc
              )
              [] li1
          with
            | [] -> Empty
            | [(k, v)] -> Leaf (h1, k, v)
            | li -> HashCollision (h1, li)
        end
  | HashCollision (h, li), BitmapIndexedNode (bitmap, base) ->
      let bit = 1 lsl (hash_fragment shift h) in
      if bitmap land bit = 0 then Empty
      else
        let n = ctpop (bitmap land (pred bit)) in
        let node = intersect_node (shift + shift_step) f t1 base.(n) in
        if is_tip_node node then node
        else BitmapIndexedNode (bit, [|node|])
  | HashCollision (h, li), ArrayNode (nb_children, children) ->
      let fragment = hash_fragment shift h in
      if children.(fragment) = Empty then Empty
      else
        let child =
          intersect_node (shift + shift_step) f t1 children.(fragment) in
        if is_tip_node child then child
        else BitmapIndexedNode (1 lsl fragment, [|child|])
  | BitmapIndexedNode (bitmap1, base1), BitmapIndexedNode (bitmap2, base2) ->
      let bitmap = bitmap1 land bitmap2 in
      if bitmap = 0 then Empty
      else begin
        match
          intersect_bitmap (shift + shift_step) f
            (bitmap_to_indices bitmap1)
            (bitmap_to_indices bitmap2)
            0 0 base1 base2 []
        with
          | [] -> Empty
          | [x] when is_tip_node x -> x
          | base -> BitmapIndexedNode (bitmap, array_of_rev_list base)
      end
  | ArrayNode (num1, children1), ArrayNode (num2, children2) ->
      intersect_array (shift + shift_step) f children1 children2
  | BitmapIndexedNode (bitmap, base), ArrayNode (_nb_children, children) ->
      intersect_bitmap_array
        (shift + shift_step) f bitmap base children
  | _, _ -> intersect_node shift (fun x y -> f y x) t2 t1

let intersect f t1 t2 =
  let t2 = copy t2 in
  intersect_node 0 f t1 t2

let rec merge_array shift f children1 children2 =
  let nb_children = ref 0
  and children = Array.make chunk Empty in
  for i = 0 to mask do
    let node = merge_node shift f children1.(i) children2.(i)
    in if node <> Empty then incr nb_children;
    children.(i) <- node
  done;
  reify_node (ArrayNode (!nb_children, children))

and merge_node shift f t1 t2 = match (t1, t2) with
  | Empty, _ -> alter_all ~mute:true (fun k v -> f k None (Some v)) t2
  | Leaf (_h, k, v), _ ->
      let flag = ref false in
      let t2 = alter_all ~mute:true
        (fun k' v' ->
          if k' = k then (flag := true; f k (Some v) (Some v'))
          else f k' None (Some v')) t2;
      in if !flag then t2 else alter_mute ~shift k (fun _ -> f k (Some v) None) t2
  | HashCollision (h, li), _ ->
      let absents = ref li in
      let t2 = alter_all ~mute:true
        (fun k' v' ->
          try
            let v = List.assoc k' li in
            absents := List.remove_assoc k' !absents;
            f k' (Some v) (Some v')
          with Not_found -> f k' None (Some v')) t2 in
      List.fold_left
        (fun acc (k, v) -> alter_mute ~shift k
          (fun _ -> f k (Some v) None) acc)
        t2 !absents
  | BitmapIndexedNode (bitmap1, base1), BitmapIndexedNode (bitmap2, base2) ->
      merge_array shift f
        (bitmap_to_array bitmap1 base1)
        (bitmap_to_array bitmap2 base2)
  | BitmapIndexedNode (bitmap, base), ArrayNode (_nb_children, children) ->
      merge_array shift f (bitmap_to_array bitmap base) children
  | ArrayNode (_, children1), ArrayNode (_, children2) ->
      merge_array shift f children1 children2
  | _, _ -> merge_node shift (fun k x y -> f k y x) t2 t1

let merge f t1 t2 = merge_node 0 f t1 t2

let union f t1 t2 = merge
  (fun k x y -> match (x, y) with
    | None, _ -> y
    | _, None -> x
    | Some v1, Some v2 -> Some (f v1 v2)) t1 t2
