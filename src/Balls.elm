module Balls exposing (Model, main)

import Angle
import Browser exposing (Document)
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Length exposing (Meters)
import Pixels
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material as Material
import Sphere3d
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type Msg
    = RotateLeft


type alias Model =
    { position : Point3d Meters WorldCoordinates
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { position = Point3d.meters 4 2 2
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RotateLeft ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Balls"
    , body =
        [ scene (camera model)
        ]
    }


camera : Model -> Camera3d Meters WorldCoordinates
camera model =
    Camera3d.perspective
        { -- Camera is at the point (4, 2, 2), looking at the point
          -- (0, 0, 0), oriented so that positive Z appears up
          viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = model.position
                , upDirection = Direction3d.positiveZ
                }

        -- The image on the screen will have a total rendered 'height'
        -- of 30 degrees; small angles make the camera act more like a
        -- telescope and large numbers make it act more like a fisheye
        -- lens
        , verticalFieldOfView = Angle.degrees 30
        }


scene : Camera3d Meters WorldCoordinates -> Html Msg
scene c =
    div [ style "border" "solid" ]
        [ -- Use the preset 'Scene3d.sunny' which handles most of the lighting details
          -- for us (creates one direct light source approximating sunlight, and
          -- another soft light source representing sky light and light reflected from
          -- surrounding objects)
          Scene3d.sunny
            { camera = c
            , clipDepth = Length.meters 1
            , dimensions = ( Pixels.int 500, Pixels.int 300 )
            , background = Scene3d.transparentBackground
            , entities = [ ground, fence1, fence2, fence3, fence4, sphereEntity ]
            , shadows = True

            -- Specify the global up direction (this controls the orientation of
            -- the sky light)
            , upDirection = Direction3d.z

            -- Specify the direction of incoming sunlight (note that this is the
            -- opposite of the direction *to* the sun)
            , sunlightDirection = Direction3d.yz (Angle.degrees -120)
            }
        ]


side =
    1.5


ground : Scene3d.Entity coordinates
ground =
    Scene3d.quad (Material.matte Color.lightYellow)
        (Point3d.meters -side -side 0)
        (Point3d.meters side -side 0)
        (Point3d.meters side side 0)
        (Point3d.meters -side side 0)


fence1 : Scene3d.Entity coordinates
fence1 =
    Scene3d.quad (Material.matte Color.lightBrown)
        (Point3d.meters -side -side 0.5)
        (Point3d.meters -side -side 0)
        (Point3d.meters -side side 0)
        (Point3d.meters -side side 0.5)


fence2 : Scene3d.Entity coordinates
fence2 =
    Scene3d.quad (Material.matte Color.lightBrown)
        (Point3d.meters side -side 0.5)
        (Point3d.meters side -side 0)
        (Point3d.meters -side -side 0)
        (Point3d.meters -side -side 0.5)


fence3 : Scene3d.Entity coordinates
fence3 =
    Scene3d.quad (Material.matte Color.lightBrown)
        (Point3d.meters side -side 0.5)
        (Point3d.meters side -side 0)
        (Point3d.meters side side 0)
        (Point3d.meters side side 0.5)


fence4 : Scene3d.Entity coordinates
fence4 =
    Scene3d.quad (Material.matte Color.lightBrown)
        (Point3d.meters side side 0.5)
        (Point3d.meters side side 0)
        (Point3d.meters -side side 0)
        (Point3d.meters -side side 0.5)


sphereEntity : Scene3d.Entity coordinates
sphereEntity =
    Scene3d.sphereWithShadow material <|
        Sphere3d.withRadius (Length.centimeters 10) (Point3d.centimeters 0 0 15)


material =
    Material.nonmetal
        { baseColor = Color.lightGreen
        , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
