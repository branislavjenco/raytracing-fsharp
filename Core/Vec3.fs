module Core.Vec3

type Vec3 = struct
    val X: double
    val Y: double
    val Z: double
    new(x, y, z) = { X = x; Y = y; Z = z }
    static member Zero = Vec3(0.0, 0.0, 0.0)
    
    static member(~-) (v: Vec3) =
        Vec3(-v.X, -v.Y, -v.Z)
    
    static member (+) (v1: Vec3, v2: Vec3) = 
        Vec3(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)

    static member (-) (v1: Vec3, v2: Vec3) = 
        Vec3(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)

    static member (*) (v: Vec3, s: double) =
        Vec3(v.X * s, v.Y * s, v.Z * s)

    static member (*) (s: double, v: Vec3) =
        Vec3(v.X * s, v.Y * s, v.Z * s)

    static member (*) (v: Vec3, s: int) =
        Vec3(v.X * float s, v.Y * float s, v.Z * float s)

    static member (*) (s: int, v: Vec3) =
        Vec3(v.X * float s, v.Y * float s, v.Z * float s)

    static member (/) (v: Vec3, s: int) =
        Vec3(v.X / float s, v.Y / float s, v.Z / float s)

    static member (/) (v: Vec3, s: double) =
        Vec3(v.X / float s, v.Y / float s, v.Z / float s)

    member v.LengthSquared =
        v.X * v.X + v.Y * v.Y + v.Z * v.Z

    member v.Length =
        sqrt v.LengthSquared

    static member Normalize (v: Vec3) =
        let len = v.Length
        if len = 0.0 then v else v / len

    static member Dot (v1: Vec3, v2: Vec3) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    static member Between (p1: Vec3, p2: Vec3) =
        p2 - p1
    


end

type Point3 = Vec3

