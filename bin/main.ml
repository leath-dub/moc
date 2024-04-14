let () =
  let user = Sys.getenv "USER" in
  Printf.printf "Hello %s! This is the Monkey programming language!\n" user;
  Printf.printf "Feel free to type in commands\n";
  Repl.start In_channel.stdin Out_channel.stdout
