#use "topfind";;
#require "curl";;

let connection = Curl.init () in
      Curl.set_httpget connection true;
      Curl.set_url connection "http://www.ic.gc.ca/app/ccc/srch/nvgt.do?profileId=&lang=eng&prtl=1&V_TOKEN=1394848516368&V_SEARCH.command=navigate&V_SEARCH.resultsJSP=%2FprflSlctn.do&V_SEARCH.docsCount=20&profile=cmpltPrfl&cntnBtn=Continue";
      Curl.perform connection;
