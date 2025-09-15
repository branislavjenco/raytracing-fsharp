module Core.Program

open Core.Color
open Core.Ray
open Core.Vec3

type HitResult = 
    | Hit of double
    | Miss


// Playing with Static Type Constraints
let inline lerp (a: ^T) (b: ^T) (t: ^U) : ^T
    when ^T : (static member (+) : ^T * ^T -> ^T)
     and ^T : (static member (-) : ^T * ^T -> ^T)
     and ^T : (static member (*) : ^T * ^U -> ^T) =
    a + (b - a) * t

// Find intersection of ray with sphere in terms of how far along the ray it is
let hitSphere(center: Point3, radius: double, r: Ray) : HitResult =

    // Vector from ray origin to sphere center
    let oc = Vec3.Between(r.Origin, center)

    // Coefficients for the quadratic equation
    let a = Vec3.Dot(r.Direction, r.Direction)
    let b = -2.0 * Vec3.Dot(r.Direction, oc)
    let c = Vec3.Dot(oc, oc) - radius*radius

    let discriminant = b*b - 4.0 * a * c
    if discriminant < 0.0 then
        Miss
    else
        Hit ((-b - sqrt discriminant) / (2.0 * a))

let rayColor(ray: Ray) : Color =
    let sphereCenter = Point3(0.0, 0.0, -1.0)
    let sphereRadius = 0.5
    let result = hitSphere(sphereCenter, sphereRadius, ray)
    match result with
    | Hit t ->
        // Subtract point of intersection with sphere center to get the normal vector (normalized)
        let N = Vec3.Between(sphereCenter, ray.At t) |> Vec3.Normalize

        // Take the normalized vector components and map them from [-1, 1] to [0, 1] to show a color
        0.5 * Color(N.X + 1.0, N.Y + 1.0, N.Z + 1.0)
        // maybe we could have a nice generic remap function but it's a little fiddly if it should work on scalars and vectors
    | Miss ->
        // No hit, lerp from blue to white vertically
        let unitDirection = Vec3.Normalize ray.Direction

        // Map the Y component from interval [-1, 1] to [0, 1] for the next step
        let a = 0.5 * (unitDirection.Y + 1.0)

        // The bigger "a" is the more blue, and vice versa
        lerp Color.White (Color(0.5, 0.7, 1.0)) a


[<EntryPoint>]
let main argv =

    let aspectRatio = 16.0 / 9.0
    let imageWidth = 256

    // Calculate the height from width and aspect ratio and ensure it's at least 1
    let imageHeight = max (int (float imageWidth / aspectRatio)) 1
    eprintfn "%A" imageHeight

    // Camera
    let focalLength = 1.0
    let viewportHeight = 2.0
    let viewportWidth = viewportHeight * (float imageWidth / float imageHeight)
    let cameraCenter = Point3.Zero

    // Calculate the vectors across the horizontal and vertical viewport edges
    let viewportU = Vec3(viewportWidth, 0.0, 0.0)
    let viewportV = Vec3(0.0, -viewportHeight, 0.0)

    // Calculate the horizontal and vertical delta vectors from pixel to pixel
    let pixelDeltaU = viewportU / imageWidth
    let pixelDeltaV = viewportV / imageHeight

    // Calculate the location of the upper left pixel
    let viewportUpperLeft = cameraCenter - Vec3(0.0, 0.0, focalLength) - viewportU/2 - viewportV/2 
    // 1 unit of distance away from the camera lies the viewport
    // half the width to the left and half the height up lies the upper left corner

    let pixel00Loc = viewportUpperLeft + 0.5 * (pixelDeltaU + pixelDeltaV)
    // the center of the uppermost left pixel is half the delta down and right


    // Render
    printfn $"P3\n{imageWidth} {imageHeight}\n255"
    for j in 0 .. imageHeight-1 do
        eprintf $"\rScanlines remaining: {imageHeight - j}"
        for i in 0 .. imageWidth-1 do
            // Each pixel center is offset from the initial upper left pixel
            let pixelCenter = pixel00Loc + i * pixelDeltaU + j * pixelDeltaV
            let rayDirection = Vec3.Between(cameraCenter, pixelCenter)
            let r = Ray(cameraCenter, rayDirection)
            let pixelColor = rayColor(r)
            WriteColor(stdout, pixelColor)
    eprintf "\rDone                    \n"
    0