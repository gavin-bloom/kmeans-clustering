open Random

type point = float array

type cluster = { mutable center : point; mutable points : point list }

let distance p1 p2 =
  let sum_sq_diff = ref 0.0 in
  Array.iter2 (fun x y -> sum_sq_diff := !sum_sq_diff +. (x -. y) ** 2.0) p1 p2;
  sqrt !sum_sq_diff

let find_nearest_cluster point clusters =
  let nearest_cluster = ref None in
  let min_distance = ref infinity in
  List.iter (fun cluster ->
    let d = distance point cluster.center in
    if d < !min_distance then (
      nearest_cluster := Some cluster;
      min_distance := d
    )
  ) clusters;
  match !nearest_cluster with
  | Some cluster -> cluster
  | None -> failwith "No clusters"

let rec update_clusters points clusters iterations max_iterations =
  (*clear clusters and reassing points*)
  List.iter (fun cluster -> cluster.points <- []) clusters;
  List.iter (fun point ->
    let nearest_cluster = find_nearest_cluster point clusters in
    nearest_cluster.points <- point :: nearest_cluster.points
  ) points;
  let converged = ref true in
  List.iter (fun cluster ->
    let old_center = cluster.center in
    match cluster.points with
    | [] -> ()
    | _ ->
      (*calculates new center for clusters*)
      let dim = Array.length old_center in
      let new_center = Array.make dim 0.0 in
      List.iter (fun point ->
        Array.iteri (fun i coord -> new_center.(i) <- new_center.(i) +. coord) point
      ) cluster.points;
      Array.iteri (fun i _ ->
        new_center.(i) <- new_center.(i) /. float_of_int (List.length cluster.points)
      ) new_center;
      cluster.center <- new_center;
      if not (old_center = new_center) then converged := false
  ) clusters;
  (*loop if not converged*)
  if not !converged && iterations + 1 < max_iterations then update_clusters points clusters (iterations + 1) max_iterations

let kmeans points k max_iterations =
  let clusters = Array.make k { center = [||]; points = [] } in
  (*init clusters with random points*)
  for i = 0 to k - 1 do
    clusters.(i) <- { center = Array.copy points.(i); points = [] }
  done;
  for i = k to Array.length points - 1 do
    let random_cluster = clusters.(Random.int k) in
    random_cluster.points <- points.(i) :: random_cluster.points
  done;
  update_clusters (Array.to_list points) (Array.to_list clusters) 0 max_iterations;
  Array.to_list clusters

let () =
  let points = [|
    [| 1.0; 2.0; 3.0 |];
    [| 4.0; 5.0; 6.0 |];
    [| 7.0; 8.0; 9.0 |];
    [| 2.0; 3.0; 4.0 |];
    [| 5.0; 6.0; 7.0 |];
    [| 8.0; 9.0; 10.0 |];
  |] in
  let k = 2 in
  let max_iterations = 10 in
  let clusters = kmeans points k max_iterations in
  List.iteri (fun i cluster ->
    Printf.printf "Center of cluster %d: (%s)\n" i (Array.fold_left (fun acc x -> acc ^ (string_of_float x) ^ ", ") "" cluster.center);
	Printf.printf "Points in cluster %d: \n" i;
    List.iter (fun point ->
		Printf.printf "(%s)\n" (Array.fold_left (fun acc x -> acc ^ (string_of_float x) ^ ", ") "" point)) cluster.points) clusters;
