open BitUtils
open Printf

let hash = Hashtbl.hash

type ('k, 'v) t =
  | Empty
  | Leaf of int * 'k * 'v
  | HashCollision of int * ('k * 'v) list
  | BitmapIndexedNode of int * ('k, 'v) t array
  | ArrayNode of int * ('k, 'v) t array

let rec show pk pv = function
  | Empty -> ()
  | Leaf (_, k, v) -> printf "(%s, %s) " (pk k) (pv v)
  | HashCollision (_, pairs) -> 
      List.iter (fun (k, v) -> printf "(%s, %s)" (pk k) (pv v)) pairs
  | BitmapIndexedNode (_, arr) -> Array.iter (show pk pv) arr
  | ArrayNode (_, arr) -> Array.iter (show pk pv) arr

let print = show (sprintf "%d") (sprintf "%d")

let empty = Empty
let leaf h k v = Leaf (h, k, v)
let hash_collision h pairs = HashCollision (h, pairs)
let bitmap_indexed_node bitmap base = BitmapIndexedNode (bitmap, base)
let array_node num_children children = ArrayNode (num_children, children)

let singleton k v = leaf (hash k) k v

let is_empty x = x = Empty

let rec cardinal = function
  | Empty -> 0
  | Leaf (_, _, _) -> 0
  | HashCollision (_, pairs) -> List.length pairs
  | BitmapIndexedNode (_, base) -> Array.fold_left (fun acc child -> acc + cardinal child) 0 base
  | ArrayNode (_, children) -> Array.fold_left (fun acc child -> acc + cardinal child) 0 children

let is_tip_node = function
  | Empty | Leaf (_, _, _) | HashCollision (_, _) -> true
  | _ -> false

let node_hash = function
  | Leaf (h, _, _) -> h
  | HashCollision (h, _) -> h
  | _ -> failwith "node_hash"

let shift_step = 5
let chunk = 1 lsl shift_step
let mask = pred chunk
let bmnode_max = 16 (* Maximum size of a BitmapIndexedNode *)
let arrnode_min = 9 (* Minimum size of an ArrayNode *)

let hash_fragment shift h = (h asr shift) land mask

let option default f = function
  | None -> default
  | Some x -> f x

let remove tab ix =
  let tab' = Array.make (Array.length tab - 1) Empty in
  Array.blit tab 0 tab' 0 ix;
  Array.blit tab (succ ix) tab' ix (Array.length tab - ix - 1);
  tab'

let add tab ix v =
  let tab' = Array.make (Array.length tab + 1) Empty in
  Array.blit tab 0 tab' 0 ix;
  Array.blit tab ix tab' (succ ix) (Array.length tab - ix);
  tab'.(ix) <- v;
  tab'
    
let set tab ix v =
  let tab' = Array.copy tab in
  tab'.(ix) <- v;
  tab'

let rec combine_leaves shift leaf1 leaf2 = 
  match (leaf1, leaf2) with
    | Leaf (h1, k1, v1), Leaf (h2, k2, v2) ->
        if h1 = h2 then HashCollision (h1, [k1, v1; k2, v2])
        else
          let sub_h1 = hash_fragment shift h1 in
          let sub_h2 = hash_fragment shift h2 in
          let (nodeA, nodeB) = 
            if sub_h1 < sub_h2 then (leaf1, leaf2) else (leaf2, leaf1) in
          let bitmap = to_bitmap sub_h1 lor to_bitmap sub_h2 in
          BitmapIndexedNode (
            bitmap, 
            if sub_h1 = sub_h2 
            then [|(combine_leaves (shift + shift_step) leaf1 leaf2)|]
            else [|nodeA; nodeB|]
          )
    |_, _ -> failwith "combine_leaves"

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
  array_node n tab
    
let pack_array_node to_remove num_children children =
  let base = Array.make num_children Empty in
  let rec loop ix jx bitmap =
    if ix = chunk then bitmap_indexed_node bitmap base
    else if
        is_empty children.(ix) || to_remove ix
    then loop (succ ix) jx bitmap
    else begin
      base.(jx) <- children.(ix);
      loop (succ ix) (succ jx) (bitmap lor (1 lsl ix))
    end
  in loop 0 0 0

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
      
let rec alter_node shift update hash key = function
  | Empty -> 
      option Empty (leaf hash key) (update None)
  | Leaf (h, k, v) as leaf1 ->
      if k = key 
      then option Empty (leaf h k) (update (Some v))
      else 
        option leaf1
          (fun x -> combine_leaves shift leaf1 (leaf hash key x))
          (update None)
  | HashCollision (h, pairs) ->
      let pairs = update_list update key pairs in begin
        match pairs with
          | [] -> failwith "alter_node" (* Should never happen *)
          | [(k, v)] -> leaf h k v
          | _ -> hash_collision h pairs
      end
  | BitmapIndexedNode (bitmap, base) as bm_node -> 
      let sub_hash = hash_fragment shift hash in
      let ix = from_bitmap bitmap sub_hash in
      let bit = to_bitmap sub_hash in
      let not_exists = bitmap land bit = 0 in
      let child = if not_exists then Empty else base.(ix) in
      let child = alter_node (shift + shift_step) update hash key child in
      begin
        match change not_exists (is_empty child) with
          | Nil -> bm_node
          | Modified -> 
              let base = Array.copy base in
              base.(ix) <- child; bitmap_indexed_node bitmap base
          | Removed ->
              let bitmap = (bitmap land (lnot bit)) land mask in
              if bitmap = 0 then Empty
              else 
                if Array.length base = 2 && is_tip_node base.(ix lxor 1)
                then base.(ix lxor 1)
                else bitmap_indexed_node bitmap (remove base ix)
          | Added -> 
              if Array.length base = bmnode_max
              then expand_bitmap_node sub_hash child bitmap base
              else bitmap_indexed_node (bitmap lor bit) (add base ix child)
      end 
  | ArrayNode (num_children, children) as arr_node ->
      let sub_hash = hash_fragment shift hash in
      let child = children.(sub_hash) in
      let child' = alter_node (shift + shift_step) update hash key child in
      match change (is_empty child) (is_empty child') with
        | Nil -> arr_node
        | Added -> array_node (succ num_children) (set children sub_hash child')
        | Modified -> array_node num_children (set children sub_hash child')
        | Removed ->
            if num_children = arrnode_min then 
              pack_array_node (( = ) sub_hash) num_children children
            else array_node (num_children - 1) (set children sub_hash Empty)
              
let alter update key root =
  alter_node 0 update (hash key) key root
    
let insert_with f k v hamt =
  alter (function | None -> Some v | Some w -> Some (f w v)) k hamt
    
let add k v t = insert_with (fun _ _ -> v) k v t
  
let update f = alter (function | None -> None | Some v -> f v)

let remove k t = alter (fun _ -> None) k t
  
let modify _k f = alter (function | None -> None | Some v -> Some (f v))

let modify_def v0 _k f = alter (function | None -> Some (f v0) | Some v -> Some (f v))

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
  
and alter_all f = function
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
  | ArrayNode (num_children, children) ->
      let children = Array.map (alter_all f) children in
      let num_children = Array.fold_left (fun n v -> if v = Empty then n else succ n) 0 children in
      if num_children < arrnode_min 
      then pack_array_node (fun _ -> false) num_children children
      else ArrayNode (num_children, children)

let map f = alter_all (fun _k v -> Some (f v))
let filter f = alter_all (fun _k v -> if f v then Some v else None)

let filter_map = alter_all

let mapi f = alter_all (fun k v -> Some (f k v))
let filteri f = alter_all (fun k v -> if f k v then Some v else None)

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
        let bit = to_bitmap sub_hash in
        if bitmap land bit = 0 then raise Not_found
        else find (shift + shift_step) hash key base.(from_bitmap bitmap sub_hash)
    | ArrayNode (_, children) ->
        let child = children.(hash_fragment shift hash) in
        if is_empty child then raise Not_found
        else find (shift + shift_step) hash key child
  in find 0 (hash key) key
                        
let mem key hamt = 
  try 
    let _ = find key hamt in true
  with 
    | Not_found -> false

let rec fold f hamt v0 =
  match hamt with
    | Empty -> v0
    | Leaf (_, k, v) -> f k v v0
    | HashCollision (_, pairs) -> 
        List.fold_right (fun (k, v) acc -> f k v acc) pairs v0
    | BitmapIndexedNode (_, base) ->
        Array.fold_right (fold f) base v0
    | ArrayNode (_, children) ->
        Array.fold_right (fold f) children v0

let to_assoc hamt = 
  fold (fun k v acc -> (k, v) :: acc) hamt []

let bindings = to_assoc

let of_assoc hamt =
  List.fold_left (fun acc (k, v) -> add k v acc) empty hamt

let for_all f hamt = 
  fold (fun k v acc -> f k v && acc) hamt true

let exists f hamt =
  fold (fun k v acc -> f k v || acc) hamt false

let rec choose = function
  | Empty -> raise Not_found
  | Leaf (_, k, v) -> (k, v)
  | HashCollision (_, li) -> List.hd li
  | BitmapIndexedNode (_, base) -> choose base.(0)
  | ArrayNode (_, children) ->
      let rec loop n = 
        if is_empty children.(n) then loop (succ n) else children.(n)
      in choose (loop 0)
      
