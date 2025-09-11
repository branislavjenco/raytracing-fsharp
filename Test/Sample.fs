module Tests

open Expecto
open Core.Vec3

[<Tests>]
let tests =
  testList "Vec3" [
    testCase "Addition" <| fun _ ->
      let v1 = Vec3(1.0, 2.0, 3.0)
      let v2 = Vec3(4.0, 5.0, 6.0)
      let v3 = v1 + v2
      Expect.equal v3 (v2 + v1) "v1 + v2 should equal v2 + v1"

    testCase "Multiplication" <| fun _ ->
      let v1 = Vec3(1.0, 2.0, 3.0)
      let s = 2.0
      Expect.equal (v1 * s) (s * v1) "v1 * s should equal s * v1"
  ]
