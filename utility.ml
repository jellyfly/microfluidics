(** Utility functions for general use.  Most of these deal with the 
   default data structures, but some require the pcre library, and some
   require the Unix module *)
(** Author: Karl Zilles, released into public domain *)

open Pcre
open Printf

(** {1:generic Generic functions:} *)

(** [ compose f g ] returns a new function that is like
 running g on the inputs and then f on the results of g *)
let compose f g = fun x -> f (g x)

(** [ fold_left_from_iterator iter ] returns a fold_left function on 
 the same datastructure that your iter function works on.  You may have
 to use it as so:

    let fold_left f = fold_left_from_iterator iter_document f

 to avoid "cannot be generalized" errors
*)
let fold_left_from_iterator iter = 
    (fun f init data -> 
        let cur = ref init in
        iter (fun el -> cur := f !cur el) data;
        !cur)

(** {1:hash Hash table functions:} *)

(** Returns the unique list of keys in a Hashtable *)
let hash_keys h = 
    Hashtbl.fold (fun key _ l ->
        if l = [] || (List.hd l) <> key then key :: l else l 
    ) h []

(** Returns the unique list of values in a Hashtable *)
let hash_values h = 
    Hashtbl.fold (fun _ value l -> 
        if not (List.mem value l) then value :: l else l) h []

(** Get a value or fail with error *)
let get_value_or_fail hash key error = 
    try Hashtbl.find hash key
    with Not_found -> raise (Failure error)

(** {1:list List functions:} *)

(** [ list_iteri f l ] runs the function f on every element of l, passing
 the 0-based index of the element, and the element itself *)
let list_iteri f l = 
    let rec list_iteri' i = function
        | [] -> ()
        | h::t -> f i h; list_iteri' (i+1) t
    in
    list_iteri' 0 l

(** [ list_mapi f l ] returns a list of the results of runing the function f on every element of l, passing the 0-based index of the element, and the element itself *)
let list_mapi f l = 
    let rec list_mapi' i = function
        | [] -> [] 
        | h::t -> (f i h)::(list_mapi' (i+1) t)
    in
    list_mapi' 0 l

(** [ list_skip n l ] returns the list l with the first n elements removed, or
 the empty list if it runs out of elements to skip *)
let rec list_skip n l =
    if n = 0 || l = [] then l else list_skip (n-1) (List.tl l)

(** [ list_first n l ] returns the first n element of list l, or as many as it
 can find  *)
let rec list_first n l =
    if n = 0 then [] else 
        match l with 
        | [] -> []
        | h::t -> h::list_first (n-1) t

(** [ assoc_merge_with_replace first second ] returns a merge of two association
 * lists with any duplicate keys using the values from the second list. *)
let assoc_merge_with_replace first second = 
    List.fold_left (fun cur entry -> 
        if not (List.mem_assoc (fst entry) second) then entry::cur else cur) 
        second (List.rev first)

(** {1:string String functions:} *)

let list_of_string s =
    let rec chars s index so_far = 
        if index < 0 then so_far 
        else chars s (index-1) (s.[index] :: so_far)
    in
    chars s ((String.length s)-1) []

(** [ string_ends_with contents end ] returns true if the last characters
 * of the string contents are the string end *)
let string_ends_with s e =
    let len_s = String.length s in
    let len_e = String.length e in
    if len_s >= len_e then
        (String.sub s (len_s - len_e) len_e) = e
    else false


(** {1:system System commands:} *)

(** [ command_to_string_list command ] runs command as an external process
 and then copies the stdout of the results into a list of strings.  Stderr
 goes to the ocaml stderr *)
let command_to_string_list command = 
    let input = Unix.open_process_in command in
    let results = ref [] in
    (try
        while true do
            results := (input_line input) :: !results
        done
    with 
        End_of_file -> ());
    List.rev !results

(** [ quiet_mkdir dir permission ] runs the standard Unix.mkdir, but 
 does not throw an exception if the directory already exists *)
let quiet_mkdir dir permission = 
    try Unix.mkdir dir permission 
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** {1:pcre Pcre tools:} *)

(** Returns the string with all leading and trailing spaces removed 
 including \160 which is some weird space like character that excel
 seems to like *)
let strip s = replace ~pat:"(^(\\s|\160|&nbsp;)+|(\\s|\160|&nbsp;)+$)" s

(** A pregenerated option list for doing caseless matches in Pcre *)
let caseless = cflags [`CASELESS]

(** {1:config Configuration file tools:} *)
let parse_config_file config_file = 
    let results = Hashtbl.create 10 in
    foreach_file [config_file] (fun _ input ->
    foreach_line ~ic:input (fun line ->
        try
            let m = extract ~pat:"(.*)=(.*)" line in
            Hashtbl.replace results (strip m.(1)) (strip m.(2));
        with Not_found -> 
            if strip line <> "" then 
                eprintf "Unable to parse configuration file line:\n%s\n"
                    line;
    ));
    results

(** {1:File tools:} *)
let file_to_string file = 
    let ic = open_in file in
    let len = in_channel_length ic in
    let result = String.create len in
    let rec readdata start =
        let read = input ic result start (len-start) in
        if read = 0 then start
        else readdata (start+read) in
    let real_length = readdata 0 in
    close_in ic;
    String.sub result 0 real_length

let string_to_file file data = 
    let oc = open_out file in
    output_string oc data;
    close_out oc

