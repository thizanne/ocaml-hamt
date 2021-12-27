open Core_bench
module IntMap = Map.Make (Int)

module Int_table = Caml.Hashtbl.Make (struct
  include Caml.Int

  let hash x = Caml.Hashtbl.hash x
end)

let rec fill n acc ~f =
  match n with 0 -> acc | _ -> fill (pred n) (f n n acc) ~f

let rec mem_all n ~f =
  match n with
  | -1 -> ()
  | n ->
      ignore (f n);
      mem_all (pred n) ~f

let () =
  let open Core in
  let args = [ 1; 10; 100; 1_000; 10_000 ] in
  Command.run
  @@ Bench.make_command
       [
         Bench.Test.create_indexed ~name:"IntMap.add" ~args (fun i ->
             Staged.stage @@ fun () ->
             ignore @@ fill i IntMap.empty ~f:IntMap.add);
         Bench.Test.create_indexed ~name:"Core - HashTbl.add" ~args (fun i ->
             Staged.stage @@ fun () ->
             ignore
             @@ fill i (Int.Table.create ()) ~f:(fun k v t ->
                    Int.Table.set t ~key:k ~data:v;
                    t));
         Bench.Test.create_indexed ~name:"Stdlib - HashTbl.add" ~args (fun i ->
             Staged.stage @@ fun () ->
             ignore
             @@ fill i (Int_table.create 1) ~f:(fun k v t ->
                    Int_table.add t k v;
                    t));
         Bench.Test.create_indexed ~name:"Hamt.Int.add" ~args (fun i ->
             Staged.stage @@ fun () ->
             ignore @@ fill i Hamt.Int.empty ~f:Hamt.Int.add);
         Bench.Test.create_indexed ~name:"IntMap.mem" ~args (fun i ->
             let map = fill i IntMap.empty ~f:IntMap.add in
             let f i = IntMap.mem i map in
             Staged.stage @@ fun () -> mem_all i ~f);
         Bench.Test.create_indexed ~name:"Core - HashTbl.mem" ~args (fun i ->
             let map =
               fill i (Int.Table.create ()) ~f:(fun k v t ->
                   Int.Table.set t ~key:k ~data:v;
                   t)
             in
             let f i = Int.Table.mem map i in
             Staged.stage @@ fun () -> mem_all i ~f);
         Bench.Test.create_indexed ~name:"Stdlib - HashTbl.mem" ~args (fun i ->
             let map =
               fill i (Int_table.create 1) ~f:(fun k v t ->
                   Int_table.add t k v;
                   t)
             in
             let f i = Int_table.mem map i in
             Staged.stage @@ fun () -> mem_all i ~f);
         Bench.Test.create_indexed ~name:"Hamt.Int.mem" ~args (fun i ->
             let map = fill i Hamt.Int.empty ~f:Hamt.Int.add in
             let f i = Hamt.Int.mem i map in
             Staged.stage @@ fun () -> mem_all i ~f);
         Bench.Test.create_indexed ~name:"IntMap.map" ~args (fun i ->
             let map = fill i IntMap.empty ~f:IntMap.add in
             Staged.stage @@ fun () -> IntMap.map (fun _ -> ()) map);
         Bench.Test.create_indexed ~name:"Hamt.Int.map" ~args (fun i ->
             let map = fill i Hamt.Int.empty ~f:Hamt.Int.add in
             Staged.stage @@ fun () -> Hamt.Int.map (fun _ -> ()) map);
         Bench.Test.create_indexed ~name:"IntMap.iter" ~args (fun i ->
             let map = fill i IntMap.empty ~f:IntMap.add in
             Staged.stage @@ fun () -> IntMap.iter (fun _ _ -> ()) map);
         Bench.Test.create_indexed ~name:"Hamt.Int.iter" ~args (fun i ->
             let map = fill i Hamt.Int.empty ~f:Hamt.Int.add in
             Staged.stage @@ fun () -> Hamt.Int.iter (fun _ _ -> ()) map);
       ]
