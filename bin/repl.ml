open In_channel
open Out_channel

let prompt = ">> "

let start ic oc =
  let rec loop () =
    output_string oc prompt;
    flush oc;

    match input_line ic with
    | Some ln -> (
      ln
      |> String.to_seq
      |> Tok.next_token
      |> Seq.iter (fun tok -> output_string oc @@ (Tok.show tok) ^ "\n");
      loop ()
    )
    | None -> ()
  in
    loop ()
