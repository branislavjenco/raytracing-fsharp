module Core.Ray

open Vec3

type Ray = struct
    val Origin: Point3
    val Direction: Vec3
    new(origin, direction) = { Origin = origin; Direction = direction }
    member this.At(t: double) : Point3 =
        this.Origin + t * this.Direction
end

