open Core
open Async

let mylog msg =
    let stdout = Lazy.force Writer.stdout in
    Writer.write stdout msg;
    Writer.flushed stdout
    >>> ignore

let send_tcp_msg ~(host: string) ~(port: int)
        ~(f: Reader.t -> Writer.t -> 'a Deferred.t) =
    Monitor.try_with
        ~run:`Now
        ~rest:`Log
        (fun () ->
            Tcp.with_connection
                (Tcp.Where_to_connect.of_host_and_port
                @@ Host_and_port.create ~host ~port)
                (fun sock r w ->
                    f r w
                    >>= fun ret ->
                    Fd.close @@ Unix.Socket.fd sock
                    >>= fun () -> return ret))
