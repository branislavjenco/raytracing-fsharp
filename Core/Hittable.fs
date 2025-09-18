module Core.Hittable

open Core.Vec3
open Core.Ray

type HitRecord = 
    {
        p: Point3
        normal: Vec3
        t: double
    }


type HitResult = 
    | Hit of HitRecord
    | Miss



type Hittable =
    abstract member Hit: r: Ray * t_min: double * t_max: double -> HitResult