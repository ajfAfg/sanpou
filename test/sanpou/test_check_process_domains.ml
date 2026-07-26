let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let check_ok input =
  let ast = parse input in
  Sanpou.Check_process_domains.check ast

let check_fails input =
  let ast = parse input in
  try
    Sanpou.Check_process_domains.check ast;
    Alcotest.fail "Expected non-constant-domain error"
  with Sanpou.Check_process_domains.Error _ -> ()

let () =
  let open Alcotest in
  run "Check_process_domains"
    [
      ( "constant",
        [
          test_case "constant process domains" `Quick (fun () ->
              (* defs of constants, comprehension binders shadowing a var,
                 and fun params shadowing a var are all constant *)
              check_ok
                "mod m {\n\
                \  var x = 1;\n\
                \  def k = 3;\n\
                \  def widen(x) = x + k;\n\
                \  procedure f() { return (); }\n\
                \  process p(self in { x in 1..widen(2) : x > 0 }) = f;\n\
                \  }");
        ] );
      ( "non_constant",
        [
          test_case "process domain reads a var" `Quick (fun () ->
              check_fails
                {|mod m {
                    var n = 2;
                    procedure f() { n = n + 1; return (); }
                    process p(self in 1..n) = f;
                  }|});
          test_case "process domain reads a var through a def" `Quick (fun () ->
              check_fails
                {|mod m {
                    var n = 2;
                    def d = n + 1;
                    procedure f() { return (); }
                    process p(self in 1..d) = f;
                  }|});
        ] );
    ]
