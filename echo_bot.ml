open Core
open Async
open Disml
open Models

let check_command (message: Message.t) =
    if message.author.username <> "echo" &&
            String.prefix message.content 1 = "!" then (
        Utils.mylog @@ sprintf "[DEBUG] recv: %s\n" message.content;

        let (cmd, rest) =
            match String.split ~on:' ' message.content with
            | [] -> ("", [])
            | x::xs -> (x, xs)
        in

        let response =
            match cmd with
            | "!ping" -> Cmd.ping ()
            | "!paown" -> return "paaaooowwnnn :elephant:"
            | "!add" -> return @@ Cmd.add ~msg:rest
            | "!air" -> Cmd.air ~msg:rest
            | "!help" -> return "!ping\n!paown\n!add\n!air\n!help"
            | x -> return @@ sprintf "command not implemented: %s" x
        in

        response
        >>| fun resp ->
        Message.reply message resp
        >>> function
        | Ok _ -> ()
        | Error _ -> raise_s @@ Sexp.of_string "(Error occured while replying)"
    )
    else return ()

let main () =
    Client.message_create := (fun msg ->
        check_command msg
        >>> ignore);
    let token = match Sys.getenv "YEET_TOKEN" with
        | None -> failwith "YEET_TOKEN not set!"
        | Some tok -> tok
    in
    Client.start token >>> ignore

let _ =
    Scheduler.go_main ~main ()
