module Core.Test

open Vec3


let test () =
    let v1 = Vec3(1.0, 2.0, 3.0)
    let v2 = Vec3(4.0, 5.0, 6.0)
    let v3 = v1 + v2
    assert (v3.X = 5.0 && v3.Y = 7.0 && v3.Z = 9.0)