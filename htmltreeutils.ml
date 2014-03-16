(** Functions to generate and search parsed html pages.  
 Uses the Nethtml module to do the heAvy lifting. *)

(** Author: Karl Zilles, released into public domain *)

open Nethtml
open Pcre
open Printf

open Utility

(** {1:Fix questionable "form" decision in dtd} *)
let fix_dtd dtd = 
    List.map (function 
        | (name, (elclass, _)) when name = "form" ->
            (name, (elclass, `Sub_exclusions( ["form"], `Flow )))
        | line -> line
    ) dtd

let my_dtd = fix_dtd relaxed_html40_dtd 

(** {1:create Create an html tree} *)

let get_parsed_html_from_channel inchannel = 
    let parsed = List.hd (parse ~dtd:my_dtd inchannel) in
    inchannel#close_in ();
    parsed

(** [ get_parsed_html file ] returns an html tree of the contents of file.
 See the Nethtml documentation for the format *)
let get_parsed_html file = 
    let inchannel = new Netchannels.input_channel (open_in file) in
    get_parsed_html_from_channel inchannel

(** [ get_parsed_html file ] returns an html tree of the contents of file.
 See the Nethtml documentation for the format *)
let get_parsed_html_from_string str = 
    let inchannel = new Netchannels.input_string str in
    get_parsed_html_from_channel inchannel

(** {1:inspect Inspecting a tag} *)

(** [ match_tag tag doc] returns true if the current tag has type "tag".  
 Example: [match_tag "td" doc] will return true if doc is a "td" tag. *)
let match_tag thistag = function
        | Element (tag,_,_) when tag = thistag -> true 
        | _ -> false

let tag_type = function
        | Element (tag,_,_) -> tag
        | _ -> raise Not_found

(** [ attribute doc attr ] returns the value of the attribute attr if it 
 exists in the current tag, or throws a [ Not_found ] exception if it doesn't *)
let attribute doc name = match doc with 
        | Element (_,attributes,_) -> 
            List.assoc name attributes 
        | _ -> raise Not_found

let as_text, as_text_list =
    let rec as_text' = function
        | Element (_,_,subdocs) -> as_text_list' subdocs
        | Data text -> replace ~pat:"(\\r|\\n)" text
    and as_text_list' l = 
        replace ~pat:"\\s+" ~templ:" " 
            (List.fold_left (fun current doc -> current ^ 
                (as_text' doc)) "" l) in
    (compose strip as_text', compose strip as_text_list')

(** [ as_text doc] returns a string of the text contents of this tag and
 all of it's subtags*)
let as_text = as_text

(** [ as_text_list doc] returns a string of the text contents of all the tags
in this list and all of their subtags*)
let as_text_list = as_text_list

let rec as_text_formatted = function
    | Element (_,_,subdocs) -> as_text_list_formatted subdocs
    | Data text -> text
and as_text_list_formatted l = 
    (List.fold_left (fun current doc -> current ^ 
        (as_text_formatted doc)) "" l)

(** [ as_html doc] returns a string of the html contents of all the tags
in this list and all of their subtags*)
let as_html_list documentlist = 
    let result = Buffer.create 2000 in
    let outbuffer = new Netchannels.output_buffer result in
    write ~dtd:my_dtd outbuffer documentlist;
    Buffer.contents result

let as_html document = as_html_list [document]

(** {1:search Search or process an html tree} *)


(** [ iter_document f doc ] runs the function f on every tag in the document *)
let rec iter_document f doc =
    f doc;
    match doc with 
        | Element (_,_,subdocs) -> 
            List.iter (iter_document f) subdocs
        | _ -> ()

let fold_left f = fold_left_from_iterator iter_document f

(** [ iter_document f doc ] runs the function f on every tag in the document.
 In addition to passing the current element to f, it also passes a list
 of all the parent tags of that element. *)
let iter_document_with_parents f doc =
    let rec iter_document_with_parents' f parents doc =
        f doc parents;
        match doc with 
            | Element (_,_,subdocs) -> 
                List.iter (iter_document_with_parents' f (doc::parents)) 
                    subdocs
            | _ -> () 
    in
    iter_document_with_parents' f [] doc 

(** [ list_tags tagname doc ] returns a list of all tags of a certain type
 in the html tree *)
let list_tags tagname doc =
    let tags = ref [] in
    iter_document (fun doc ->
        if match_tag tagname doc then
            tags := doc :: !tags) doc;
    List.rev !tags

(** [ list_tags_with_parents tagname doc ] returns a list of all tags of a
 certain type in the html tree, and also includes a list of their parents
 with each one. *)
let list_tags_with_parents tagname doc =
    let tags = ref [] in
    iter_document_with_parents (fun doc parents ->
        if match_tag tagname doc then
            tags := (doc,parents) :: !tags) doc;
    List.rev !tags


(** [ find filter doc ] returns the first element that filter returns
 true on, searching the document in a depth first search. Raises a [ Not_found ]
 exception if nothing is matched. *)
let find filter document = 
    let rec find' filter document =
        if filter document then
            Some document
        else
            match document with
                | Element (_,_,subdocs) -> 
                    let rec loop  = (function 
                        | h::t ->
                            (match find' filter h with
                                | Some x as result -> result
                                | None -> loop t)
                        | [] -> None) in
                    loop subdocs
                | _ -> None in

    match find' filter document with
        | Some x -> x
        | None -> raise Not_found

(** [ find filter doc ] returns the first element that filter returns
 true on, searching the document in a depth first search.  Note that in
 this function we also pass along a list of parents to the filter, and
 return the list of parents with the matching object.  Raises a [ Not_found ]
 exception if nothing is matched. *)
let find_with_parent filter document = 
    let rec find' filter parents document =
        if filter parents document then
            Some (document,parents)
        else
            match document with
                | Element (_,_,subdocs) -> 
                    let add_parents = document :: parents in
                    let rec loop  = (function 
                        | h::t ->
                            (match find' filter add_parents h with
                                | Some x as result -> result
                                | None -> loop t)
                        | [] -> None) in
                    loop subdocs
                | _ -> None in

    match find' filter [] document with
        | Some x -> x
        | None -> raise Not_found

(** [ parse_tags_at_same_level tag doc]  looks down from the passed document
 and finds the first matching tag, then returns a list of
 matching tags with the same parent as the first match. *)
let parse_tags_at_same_level tag document = 
    let is_tag = match_tag tag in
    let first_tr, parents = find_with_parent (function _ -> is_tag) 
        document in
    let siblings = match List.hd parents with 
        Element (_,_,x) -> x | _ -> assert false in
    List.filter is_tag siblings

(** [parse_trs doc] calls parse_tags_at_same_level matching "tr" tags *)
let parse_trs = parse_tags_at_same_level "tr"

(** [parse_tds doc] calls parse_tags_at_same_level matching "td" tags *)
let parse_tds = parse_tags_at_same_level "td"

let get_tag_with_name tag name document = 
    find (
        fun el -> 
            try match_tag tag el && (attribute el "name")=name 
            with Not_found -> false
    ) document

(* form handling *)
let decode = Netencoding.Html.decode_to_latin1 

let get_form = get_tag_with_name "form"

let get_select = get_tag_with_name "select"

let select_selection selection =
    try
        (* look for a "selected" value *)
        find (fun el -> 
            try ignore (attribute el "selected"); true 
            with Not_found-> false) selection
    with Not_found -> 
        (* otherwise, take first option element *)
        find (match_tag "option") selection
       
let select_value selection =
	decode (attribute (select_selection selection) "value")
let select_label selection =
	decode (as_text (select_selection selection)) 

let get_select_value form name =
	select_value (get_select name form)
let get_select_label form name =
	select_label (get_select name form)

let input_value input = 
	decode (attribute input "value")
let get_input_value form name = 
    input_value (get_tag_with_name "input" name form) 

let textarea_value textarea = 
	decode (as_text_formatted textarea)
let get_textarea_value form name = 
	textarea_value (get_tag_with_name "textarea" name form)

let element_name default el = try attribute el "name" 
    with Not_found -> default

let list_forms document = 
    list_tags "form" document

let form_names document = 
    List.map (element_name "[No name]") (list_forms document)

let element_to_accessor = [
    "select", select_value;
    "input", input_value;
    "textarea", textarea_value;
]

let get_field_list form = 
    List.rev (fold_left (fun current el ->
        try
            let accessor = List.assoc (tag_type el) element_to_accessor in
(*             printf "%s: %s \n" (tag_type el) (element_name "noname" el); *)
            ( element_name "noname" el, accessor el ) :: current
        with Not_found -> current
    ) [] form)

let print_field_list a = 
    printf "[\n";
    List.iter (function name,value ->
        printf "    \"%s\",\"%s\";\n" (String.escaped name) (String.escaped value)
    ) a;
    printf "]\n"
