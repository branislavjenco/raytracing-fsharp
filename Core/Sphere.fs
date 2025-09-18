module Core.Sphere

open Core.Vec3
open Core.Ray
open Core.Hittable

type Sphere = struct 
    val Center: Point3
    val Radius: double

    new(center: Point3, radius: double) = 
        { Center = center; Radius = radius }

    interface Hittable with
        member this.Hit(r: Ray, t_min: double, t_max: double) : HitResult =
            let oc = Vec3.Between(r.Origin, this.Center)

            let a = r.Direction.LengthSquared
            let h = Vec3.Dot(r.Direction, oc)
            let c = oc.LengthSquared - this.Radius*this.Radius

            let discriminant = h*h - a*c

            if discriminant < 0.0 then
                Miss
            else
                let sqrtd = sqrt discriminant
                let root1 = (h - sqrtd) / a
                let root2 = (h + sqrtd) / a

                // This needs to be made nicer
                if root1 <= t_min || t_max <= root1 then
                    if root2 <= t_min || t_max <= root2 then 
                        Miss
                    else
                        let p = r.At root2
                        Hit { t = root2; p = p; normal = Vec3.Between(this.Center, p) / this.Radius }
                else
                    let p = r.At root1
                    Hit { t = root1; p = p; normal = Vec3.Between(this.Center, p) / this.Radius }
end