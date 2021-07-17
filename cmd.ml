open Core
open Async

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
