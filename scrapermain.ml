#use "topfind";;
#require "netcgi2";;
#require "curl";;

exception IO_ERROR
(*let uri = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"*)
let uri = "http://help.websiteos.com/websiteos/example_of_a_simple_html_page.htm"
let string_of_uri uri =
    print_string "hello me"; 
    try let connection = Curl.init () and write_buff = Buffer.create 1763 in
        Curl.set_writefunction connection
                (fun x -> Buffer.add_string write_buff x; String.length x);
        Curl.set_url connection uri;
        Curl.perform connection;
        Curl.global_cleanup ();
       	Buffer.contents write_buff
with _ -> raise (IO_ERROR);;

let parse_html_string uri = 
	let ch =new Netchannels.input_string (string_of_uri uri) in
	let docs = Nethtml.parse ?return_pis:(Some false) ch in
 	ch # close_in ();
	docs;;

parse_html_string uri;;
