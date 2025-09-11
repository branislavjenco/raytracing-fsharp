module Core.Program

open Core.Color
open Core.Ray
open Core.Vec3

let rayColor(r: Ray) : Color =
    Color.White

[<EntryPoint>]
let main argv =

    let aspectRatio = 16.0 / 9.0
    let imageWidth = 256

    // Calculate the image height and ensure it's at least 1
    let mutable imageHeight = int (float imageWidth / aspectRatio) // I'm sure there's a nicer way to do this
    imageHeight <- max imageHeight 1

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

    let pixel00Loc = viewportUpperLeft + 0.5 * (pixelDeltaU + pixelDeltaV)

    // Render

    printfn $"P3\n{imageWidth} {imageHeight}\n255"
    for j in 0 .. imageHeight-1 do
        eprintf $"\rScanlines remaining: {(imageHeight - j)}"
        for i in 0 .. imageWidth-1 do
            let pixelCenter = pixel00Loc + (i * pixelDeltaU) + (j * pixelDeltaV)
            let rayDirection = pixelCenter - cameraCenter
            let r = Ray(cameraCenter, rayDirection)
            let pixelColor = rayColor(r)
            WriteColor(stdout, pixelColor)
    eprintf "\rDone                    \n"
    0