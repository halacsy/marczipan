open OUnit
let tests = "Project Marcipan" >:::
  [
    Fsl_test.suite
  ]

let _ = run_test_tt ~verbose:true tests

