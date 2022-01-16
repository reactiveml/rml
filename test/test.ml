let () =
    Alcotest.run "Rml ppx" [
      ("Await_rml", Await_ocaml.test_set);
    ]