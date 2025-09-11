module Core.Color
open System.IO


type Color = struct
    val R: double
    val G: double
    val B: double
    new(r, g, b) = { R = r; G = g; B = b }
    static member Black = Color(0.0, 0.0, 0.0)

    static member White = Color(1.0, 1.0, 1.0)

end

let WriteColor(writer: TextWriter, pixelColor: Color) : unit =
    let rbyte = int (255.999 * pixelColor.R)
    let gbyte = int (255.999 * pixelColor.G)
    let bbyte = int (255.999 * pixelColor.B)
    fprintfn writer $"{rbyte} {gbyte} {bbyte}"