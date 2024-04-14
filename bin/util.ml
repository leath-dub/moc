let take_until p seq = seq
  |> Seq.take_while p
  |> String.of_seq

let find_advance_buf p xs =
  let rec loop p xs buf =
    let open Seq in
    match xs () with
    | Nil -> (None, xs, buf)
    | Cons (x, xs) ->
      if p x then (Some x, xs, buf) else loop p xs (buf @ [x]) in
  loop p xs []
(** Like find however it will return the sequence and visited item back at the position after the found item *)

