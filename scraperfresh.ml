#use "topfind";;
#require "str";;

let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None);;

(*let print x = 
	List.find (fun line -> 
		try 
			Str.search_forward (Str.regexp "value") line 0;
			true
		with Not_found -> false
	);;
*)

let print x = print_endline x;;
  (* for now just prints something. We want to do the matching here*)

let parse str = 
	try
	   Str.search_forward (Str.regexp ".*value*.") str 0;
  	   let s = Str.matched_string str in
	   	print_endline s
	with Not_found -> print_endline "value not found";;

let in_channel = open_in "test_html" in
try
   Stream.iter 
	(fun x -> parse x)
	(line_stream_of_channel in_channel);
   close_in in_channel
with e ->
   close_in in_channel;
   raise e
