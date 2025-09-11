module Core.Color
open System.IO


type Color = struct
    val R: double
    val G: double
    val B: double
    new(r, g, b) = { R = r; G = g; B = b }
    static member Black = Color(0.0, 0.0, 0.0)

    static member White = Color(1.0, 1.0, 1.0)

    static member (+) (v1: Color, v2: Color) = 
        Color(v1.R + v2.R, v1.G + v2.G, v1.B + v2.B)

    static member (*) (v: Color, s: double) =
        Color(v.R * s, v.G * s, v.B * s)

    static member (*) (s: double, v: Color) =
        Color(v.R * s, v.G * s, v.B * s)

    static member (*) (v: Color, s: int) =
        Color(v.R * float s, v.G * float s, v.B * float s)

    static member (*) (s: int, v: Color) =
        Color(v.R * float s, v.G * float s, v.B * float s)

end

let WriteColor(writer: TextWriter, pixelColor: Color) : unit =
    let rbyte = int (255.999 * pixelColor.R)
    let gbyte = int (255.999 * pixelColor.G)
    let bbyte = int (255.999 * pixelColor.B)
    fprintfn writer $"{rbyte} {gbyte} {bbyte}"