open Raylib

(*types*)
type case=
	|Dead
	|Alive
;;

(*global values*)
let nb_x = 100;;
let nb_y = 50;;
let px = ref 8;;



(*functionnal*)
let window_init () = (*initializes the window*)
	init_window 1500 900 "Conway's Game of Life";
	clear_background Color.black;
	set_target_fps 60
;;


let alive_neighbors m x y =
	let count = ref 0 in
	if fst m.(x-1).(y-1) = Alive then count := !count + 1;
	if fst m.(x).(y-1) = Alive then count := !count + 1;
	if fst m.(x+1).(y-1) = Alive then count := !count + 1;
	if fst m.(x-1).(y) = Alive then count := !count + 1;
	if fst m.(x+1).(y) = Alive then count := !count + 1;
	if fst m.(x-1).(y+1) = Alive then count := !count + 1;
	if fst m.(x).(y+1) = Alive then count := !count + 1;
	if fst m.(x+1).(y+1) = Alive then count := !count + 1;
	!count
;;


let add_unique t n =
	let flag = ref false in
	for i = 0 to Array.length t - 1 do
		if t.(i) = n then
			flag := true
	done;
	if !flag then
		t
	else
		Array.append t [|n|]
;;


let update_cells_cache t m =
	let new_cache = t in
	for i = 1 to nb_x - 2 do
		for j = 1 to Array.length t.(i) - 1 do (*marker*)
			let j_m = t.(i).(j) in
			let state = fst m.(i).(j_m) in
			let new_state =
				match state, alive_neighbors m i j_m with
				|Dead, 3 ->
					new_cache.(i-1) <- add_unique new_cache.(i-1) (j_m-1);
					new_cache.(i-1) <- add_unique new_cache.(i-1) (j_m);
					new_cache.(i-1) <- add_unique new_cache.(i-1) (j_m+1);
					new_cache.(i) <- add_unique new_cache.(i) (j_m-1);
					new_cache.(i) <- add_unique new_cache.(i) (j_m+1);
					new_cache.(i+1) <- add_unique new_cache.(i+1) (j_m-1);
					new_cache.(i+1) <- add_unique new_cache.(i+1) (j_m);
					new_cache.(i+1) <- add_unique new_cache.(i+1) (j_m+1);
					Alive
				|Dead, _ -> Dead
				|Alive, 2 | Alive, 3 -> Alive
				|Alive, _ ->
					Dead
			in
			m.(i).(j_m) <- (state, new_state)
		done
	done;
	for i = 1 to nb_x - 2 do
		for j = 1 to Array.length t.(i) - 2 do
			let j_m = t.(i).(j) in
			let state = snd m.(i).(j_m) in
			m.(i).(j_m) <- state, state
		done
	done;
	new_cache
;;


let print_cache cache =
	for i = 0 to Array.length cache -1 do
		for j = 0 to (Array.length cache.(i)) - 1 do
			print_int cache.(i).(j);
			print_string " "
		done;
		print_endline ""
	done;
	print_endline ""
;;




(*drawing functions*)
let draw_board m = (*draws the games*)
	for i = 0 to nb_x - 1 do
		for j = 0 to nb_y - 1  do
			let col = match m.(i).(j) with
						 |Dead, _ -> Color.create 0 0 255 255
						 |Alive, _ -> Color.create 255 255 255 255
			in
			draw_rectangle (i * !px) (j * !px) !px !px col
		done
	done
;;


let draw_cache t m =
	for i = 0 to nb_x - 1 do
		for j = 0 to Array.length t.(i) - 1 do
			let col = match m.(i).(t.(i).(j)) with
						 |Dead, _ -> Color.create 0 0 255 255
						 |Alive, _ -> Color.create 255 255 255 255
			in
			draw_rectangle (i * !px) (t.(i).(j) * !px) !px !px col
		done
	done
;;





let rec loop m cache= (*loop function*)
	match window_should_close () with
	| true -> close_window ()
	| false ->
		let new_cache = match is_key_down H with
			|true ->	update_cells_cache cache m
			|false -> cache
		in
		print_cache new_cache;
		draw_cache new_cache m;
		begin_drawing ();
		end_drawing ();
		loop m new_cache
;;



let _ = (*init function*)
	window_init ();
	let m = Array.make_matrix nb_x nb_y (Dead, Dead) in
	m.(3).(6) <- (Alive, Alive);
	m.(4).(7) <- (Alive, Alive);
	m.(4).(8) <- (Alive, Alive);
	m.(3).(8) <- (Alive, Alive);
	m.(2).(8) <- (Alive, Alive);
	m.(7).(12) <- (Alive, Alive);
	m.(8).(12) <- (Alive, Alive);
	m.(9).(12) <- (Alive, Alive);
	m.(10).(12) <- (Alive, Alive);
	m.(10).(11) <- (Alive, Alive);
	m.(10).(10) <- (Alive, Alive);
	m.(9).(9) <- (Alive, Alive);
	m.(6).(11) <- (Alive, Alive);
	m.(6).(9) <- (Alive, Alive);
	let cache = Array.make nb_x [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15|] in
	draw_board m;
	loop m cache
;;
