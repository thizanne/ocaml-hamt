open Core_bench
module IntMap = Map.Make (Int)

module Int_table = Caml.Hashtbl.Make (struct
  include Caml.Int

  let hash x = Caml.Hashtbl.hash x
end)

let rec fill n acc ~f =
  match n with 0 -> acc | _ -> fill (pred n) (f n n acc) ~f

let () =
  let open Core in
  let args = [ 1; 10; 100; 1000; 10_000 ] in
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
       ]
