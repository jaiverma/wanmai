open Core
open Async
open Disml
open Models

let add ~(msg: string list) =
    let nums =  List.map ~f:int_of_string msg in
    if List.length nums <> 2 then "usage: !add <a> <b>"
    else
        nums
        |> List.fold_left ~init:0 ~f:(+)
        |> string_of_int

let air ~(msg: string list) =
    let cmd =
        match msg with
        | x :: _ -> x
        | _ -> ""
    in

    let cmd =
        match cmd with
        | "on" -> "on"
        | "off" -> "off"
        | _ -> ""
    in

    match cmd with
    | "on" | "off" -> (
        Utils.send_tcp_msg ~host:"127.0.0.1" ~port:10001 ~f:(fun _r w ->
            Writer.write w cmd;
            Writer.flushed w
            >>= fun () ->
            return "done")
        >>| function
        | Ok s -> s
        | Error _ -> "error")

    | _ -> return "usage: !air on/off"

let ping () =
    Utils.send_tcp_msg ~host:"10.0.0.5" ~port:10002 ~f:(fun _r w ->
            Writer.write w "HELLO";
            Writer.flushed w
            >>= fun () ->
            return "paging...!")
    >>| function
    | Ok s -> s
    | Error _ -> "error"

let check_command (message: Message.t) =
    if message.author.username <> "echo" && String.prefix message.content 1 = "!" then (
        Utils.mylog @@ sprintf "[DEBUG] recv: %s\n" message.content;

        let (cmd, rest) =
            match String.split ~on:' ' message.content with
            | [] -> ("", [])
            | x::xs -> (x, xs)
        in

        let response =
            match cmd with
            | "!ping" -> ping ()
            | "!paown" -> return "paaaooowwnnn :elephant:"
            | "!add" -> return @@ add ~msg:rest
            | "!air" -> air ~msg:rest
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
