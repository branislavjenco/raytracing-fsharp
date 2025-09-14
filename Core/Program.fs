module Core.Program

open Core.Color
open Core.Ray
open Core.Vec3

let hitSphere(center: Point3, radius: double, r: Ray) : double =
    let oc = center - r.Origin
    let a = Vec3.Dot(r.Direction, r.Direction)
    let b = -2.0 * Vec3.Dot(r.Direction, oc)
    let c = Vec3.Dot(oc, oc) - radius*radius
    let discriminant = b*b - 4.0 * a * c
    if discriminant < 0.0 then
        -1.0
    else
        (-b - sqrt discriminant) / (2.0 * a)

let rayColor(r: Ray) : Color =
    let t = hitSphere(Point3(0.0, 0.0, -1.0), 0.5, r)
    if t > 0.0 then
        let N = Vec3.Normalize (r.At(t) - Point3(0.0, 0.0, -1.0))
        0.5 * Color(N.X + 1.0, N.Y + 1.0, N.Z + 1.0)
    else
        let unitDirection = Vec3.Normalize r.Direction
        let a = 0.5 * (unitDirection.Y + 1.0)
        (1.0 - a) * Color.White + a * Color(0.5, 0.7, 1.0)


[<EntryPoint>]
let main argv =

    let aspectRatio = 16.0 / 9.0
    let imageWidth = 256

    // Calculate the image height and ensure it's at least 1
    let imageHeight = max (int (float imageWidth / aspectRatio)) 1

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
        eprintf $"\rScanlines remaining: {(imageHeight - j)}"
        for i in 0 .. imageWidth-1 do
            // Each pixel center is offset from the initial upper left pixel
            let pixelCenter = pixel00Loc + i * pixelDeltaU + j * pixelDeltaV
            let rayDirection = pixelCenter - cameraCenter
            let r = Ray(cameraCenter, rayDirection)
            let pixelColor = rayColor(r)
            WriteColor(stdout, pixelColor)
    eprintf "\rDone                    \n"
    0