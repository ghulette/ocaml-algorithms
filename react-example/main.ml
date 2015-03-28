Nettls_gnutls.init () ;;
open Printf
open Nethttp_client.Convenience

module Json = Yojson.Basic

let _ =
  let resp = http_get "https://api.github.com/users" in
  let json = Json.from_string resp in
  let open Yojson.Basic.Util in
  let users = convert_each (fun u -> (member "login" u |> to_string, 
                                      member "url" u |> to_string)) json in
  List.iter (fun (usr,url) -> printf "%s:%s\n" usr url) users
