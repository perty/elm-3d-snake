module Main exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}

import Angle exposing (Angle)
import Array
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length
import LuminousFlux
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import Triangle3d
import TriangularMesh
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Bool -- Whether the mouse button is currently down
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)


{-| This is a slightly modified pyramid (taller, to make for a more
interesting shadow).
-}
pyramidMesh : Mesh.Uniform WorldCoordinates
pyramidMesh =
    let
        frontLeft =
            Point3d.centimeters 10 10 0

        frontRight =
            Point3d.centimeters 10 -10 0

        backLeft =
            Point3d.centimeters -10 10 0

        backRight =
            Point3d.centimeters -10 -10 0

        tip =
            Point3d.centimeters 0 0 20

        triangularMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ frontLeft
                    , frontRight
                    , backLeft
                    , backRight
                    , tip
                    ]
                )
                [ ( 1, 0, 4 )
                , ( 0, 2, 4 )
                , ( 2, 3, 4 )
                , ( 3, 1, 4 )
                , ( 1, 3, 0 )
                , ( 0, 3, 2 )
                ]
    in
    Mesh.indexedFacets triangularMesh


{-| In addition to constructing a Mesh value, we need to construct a special
Shadow value from that mesh. As with meshes, Shadow values should be created
once and then saved in your model.
-}
pyramidShadow : Mesh.Shadow WorldCoordinates
pyramidShadow =
    Mesh.shadow pyramidMesh



-- Use Scene3d.meshWithShadow instead of Scene3d.mesh, passing the
-- Shadow value that we created


pyramidEntity : Scene3d.Entity WorldCoordinates
pyramidEntity =
    Scene3d.meshWithShadow (Material.matte Color.blue) pyramidMesh pyramidShadow



-- Create a simple 'floor' object to cast a shadow onto


floor : Scene3d.Entity coordinates
floor =
    Scene3d.quad (Material.matte Color.darkGrey)
        (Point3d.centimeters 25 30 -5)
        (Point3d.centimeters -30 30 -5)
        (Point3d.centimeters -30 -25 -5)
        (Point3d.centimeters 25 -25 -5)



-- Create a point light representing an incandescent (tungsten) light
-- bulb, and specify that it should cast shadows. Only up to four lights
-- in a given scene can cast shadows, and casting shadows is relatively
-- expensive, so try to limit the number of shadows and shadow-casting
-- lights in a given scene.


lightBulb =
    Light.point (Light.castsShadows True)
        { position = Point3d.centimeters 50 -40 50
        , chromaticity = Light.incandescent -- color of the light
        , intensity = LuminousFlux.lumens 400 -- total light 'power'
        }



-- Create some soft lighting to fill in shadowed areas


softLighting =
    Light.overhead
        { upDirection = Direction3d.z
        , chromaticity = Light.incandescent
        , intensity = Illuminance.lux 50
        }


init : () -> ( Model, Cmd Msg )
init () =
    -- Create a couple of Mesh values containing a single triangle each and
    -- store them in the model
    let
        mesh1 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 0 0)
                    (Point3d.meters 1 1 0)
                ]

        mesh2 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 1 0)
                    (Point3d.meters 0 1 0)
                ]
    in
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        Browser.Events.onMouseDown (Decode.succeed MouseDown)


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a viewpoint by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0.5 0.5 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 3
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "Snake 3D"
    , body =
        [ scene camera
        ]
    }


scene : Camera3d.Camera3d Length.Meters WorldCoordinates -> Html msg
scene camera =
    Scene3d.custom
        { camera = camera
        , clipDepth = Length.meters 0.1
        , dimensions = ( Pixels.int 400, Pixels.int 300 )
        , background = Scene3d.transparentBackground
        , entities = [ pyramidEntity, floor ]

        -- Define the lights to use in the scene. elm-3d-scene only supports up
        -- to eight total lights, so there are different functions for different
        -- numbers of lights instead of a single function taking a list.
        , lights = Scene3d.twoLights lightBulb softLighting

        -- This is a reasonably typical exposure value for an interior home
        -- scene; see https://en.wikipedia.org/wiki/Exposure_value#Tabulated_exposure_values
        -- for some representative values for different types of scenes
        , exposure = Scene3d.exposureValue 5

        -- White balance specifies what color shows up as white in the rendered
        -- scene; this should usually be set to the dominant light color
        , whiteBalance = Light.incandescent

        -- When using Scene3d.custom, we have to explicitly specify what kind of
        -- antialiasing (if any) to use
        , antialiasing = Scene3d.multisampling

        -- Similarly, we have to specify what kind of tone mapping (if any) to
        -- use; see the ExposureAndToneMapping example for details
        , toneMapping = Scene3d.noToneMapping
        }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
