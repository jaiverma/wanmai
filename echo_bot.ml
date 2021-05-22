open Async
open Core
open Disml
open Models

let check_command (message: Message.t) =
    printf "[DEBUG] recv: %s\n" message.content;
    if String.is_prefix ~prefix:"!ping" message.content then
        Message.reply message "Pong!" >>> ignore

let main () =
    Client.message_create := check_command;
    let token = match Sys.getenv "YEET_TOKEN" with
        | None -> failwith "YEET_TOKEN not set!"
        | Some tok -> tok
    in
    Client.start token >>> ignore

let _ =
    Scheduler.go_main ~main ()
