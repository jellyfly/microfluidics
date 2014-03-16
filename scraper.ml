let result = Buffer.create 2000 in 
let write s = Buffer.add_string result s in   
let connection = Curl.init () in
      Curl.set_httpget connection true;
      Curl.set_url connection "http://tycho.usno.navy.mil/cgi-bin/timer.pl";
      Curl.set_writefunction connection write;
      Curl.set_headerfunction connection (fun s-> ());
      Curl.perform connection;
      Curl.cleanup connection;;
