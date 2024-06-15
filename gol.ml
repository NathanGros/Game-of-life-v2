open Raylib
open Random



let grid_w = 150;;
let grid_h = 350;;
let display_grid = ref false;;
let running = ref true;;

let bg_color = Color.create 66 66 70 255;;
let dead_color = Color.create 100 100 105 255;;
let alive_color = Color.create 190 191 196 255;;


(*functions*)

let window_init () =
	init_window 0 0 "Game of life";
	clear_background bg_color;
	set_window_state [Window_resizable];
	set_target_fps 500
;;


let draw t =
	let w = get_screen_width () in
	let h = get_screen_height () in
	let px = min ((w-200) / grid_w) ((h-200) / grid_h) in
	let start_x = h / 2 - px * grid_h / 2 in
	let start_y = w / 2 - px * grid_w / 2 in

	clear_background bg_color;
	for i = 0 to grid_h - 1 do
		for j = 0 to grid_w - 1 do
			let cell_size = match !display_grid with |true -> px - 1 |false -> px in
			if t.(i).(j) = 1 then
				draw_rectangle (start_y + j * px) (start_x + i * px) cell_size cell_size alive_color
			else
				draw_rectangle (start_y + j * px) (start_x + i * px) cell_size cell_size dead_color
		done
	done
;;


let neighbors t x y =
	match x>0, x<grid_h-1, y>0, y<grid_w-1 with
	|true, true, true, true -> t.(x+1).(y-1) + t.(x+1).(y) + t.(x+1).(y+1) + t.(x).(y-1) + t.(x).(y+1) + t.(x-1).(y+1) + t.(x-1).(y) + t.(x-1).(y-1)

	|true, true, true, false-> t.(x+1).(y-1) + t.(x+1).(y) + t.(x).(y-1) + t.(x-1).(y) + t.(x-1).(y-1)
	|true, true, false, true -> t.(x+1).(y) + t.(x+1).(y+1) + t.(x).(y+1) + t.(x-1).(y+1) + t.(x-1).(y)
	|true, false, true, true -> t.(x).(y-1) + t.(x).(y+1) + t.(x-1).(y+1) + t.(x-1).(y) + t.(x-1).(y-1)
	|false, true, true, true -> t.(x+1).(y-1) + t.(x+1).(y) + t.(x+1).(y+1) + t.(x).(y-1) + t.(x).(y+1)

	|true, false, true, false-> t.(x).(y-1) + t.(x-1).(y) + t.(x-1).(y-1)
	|true, false, false, true -> t.(x).(y+1) + t.(x-1).(y+1) + t.(x-1).(y)
	|false, true, false, true -> t.(x+1).(y) + t.(x+1).(y+1) + t.(x).(y+1)
	|false, true, true, false-> t.(x+1).(y-1) + t.(x+1).(y) + t.(x).(y-1)

	|_, _, _, _ -> failwith "error"
;;


let update t =
	let t2 = Array.make_matrix grid_h grid_w 0 in
	for i = 0 to grid_h - 1 do
		for j = 0 to grid_w - 1 do
			t2.(i).(j) <- match t.(i).(j), neighbors t i j with
					|0, 3 -> 1
					|0, _ -> 0
					|_, 2 |_, 3 -> 1
					|_, _ -> 0
			;
		done
	done;
	t2
;;



(*main*)

let rec loop t =
	match window_should_close () with
	| true -> close_window ()
	| false ->
		if !running then draw t;

		if is_key_pressed Space then running := not !running;

		if is_key_pressed G then (display_grid := not !display_grid; draw t);

		if is_key_pressed C then begin
			for i = 0 to grid_h - 1 do
				for j = 0 to grid_w - 1 do
					t.(i).(j) <- 0
				done
			done;
			draw t
		end;

		if is_key_pressed R then begin
			Random.self_init ();
			for i = 0 to grid_h - 1 do
				for j = 0 to grid_w - 1 do
					t.(i).(j) <- Random.int 2
				done
			done;
			draw t
		end;

		if is_mouse_button_down Left then begin
			let w = get_screen_width () in
			let h = get_screen_height () in
			let px = min ((w-200) / grid_w) ((h-200) / grid_h) in
			let start_x = h / 2 - px * grid_h / 2 in
			let start_y = w / 2 - px * grid_w / 2 in
			let mx = get_mouse_x () in
			let my = get_mouse_y () in
			if mx > start_y && my > start_x && mx < w - start_y && my < h - start_x then (
				t.((my - start_x) / px).((mx - start_y) / px) <- 1;
				draw t)
		end;

		if is_mouse_button_down Right then begin
			let w = get_screen_width () in
			let h = get_screen_height () in
			let px = min ((w-200) / grid_w) ((h-200) / grid_h) in
			let start_x = h / 2 - px * grid_h / 2 in
			let start_y = w / 2 - px * grid_w / 2 in
			let mx = get_mouse_x () in
			let my = get_mouse_y () in
			if mx > start_y && my > start_x && mx < w - start_y && my < h - start_x then (
				t.((my - start_x) / px).((mx - start_y) / px) <- 0;
				draw t)
		end;

		begin_drawing ();
		end_drawing ();
		match !running with
		|true -> loop (update t)
		|false -> loop t
;;

let _ =
	window_init ();
	let t = Array.make_matrix grid_h grid_w 0 in
	for i = 0 to grid_h - 1 do
		for j = 0 to grid_w - 1 do
			t.(i).(j) <- Random.int 2
		done
	done;
	loop t
;;
