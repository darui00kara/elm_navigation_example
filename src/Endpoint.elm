module Endpoint exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import List exposing (..)

main =
  Navigation.program UrlChange
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }

-- User

type alias UserSchema =
  { id : Int
  , name : String
  , email : String
  }

type UserAction =
  Index
  | Show Int
  | New
  | Create UserSchema
  | Edit
  | Update UserSchema
  | Delete Int

initUser : UserSchema 
initUser =
  { id = 0, name = "empty", email = "empty" }

userAction : UserAction -> UserSchema -> ( UserSchema, Cmd Msg )
userAction action user =
  case action of
    Index ->
      ( user, Cmd.none )
    Show id ->
      ( { user | id = id }, Cmd.none )
    New ->
      ( user, Cmd.none )
    Create newUser ->
      ( newUser, Cmd.none )
    Edit ->
      ( user, Cmd.none )
    Update editUser ->
      ( editUser, Cmd.none )
    Delete id ->
      ( initUser, Cmd.none )

-- userRender : Msg -> UserSchema -> Html Msg

-- static page

type StaticPageAction =
  Home
  | Help
  | About
  | Contact

staticPageAction : StaticPageAction -> Model -> ( Model, Cmd Msg )
staticPageAction action model =
  case action of
    _ ->
      ( model, Cmd.none )

-- Router

type alias RoutePath =
  List String

fromUrlHash : String -> RoutePath
fromUrlHash routePath =
  routePath |> String.split "/" |> drop 1

routing : Navigation.Location -> Model -> ( Model, Cmd Msg )
routing location model =
  let
    routingTarget = matcher (fromUrlHash location.hash) model
  in
    case routingTarget of
      StaticPageRoute action ->
        staticPageAction action model
      UserRoute action ->
        let
          ( schema, cmd ) = userAction action model.user
        in
          ( { model | user = schema }, cmd )
      NotFound ->
        ( model, Cmd.none )

type RoutingTarget =
  NotFound
  | StaticPageRoute StaticPageAction
  | UserRoute UserAction 

matcher : RoutePath -> Model -> RoutingTarget
matcher routePath model =
  case routePath of
    [ ] ->
      StaticPageRoute Home
    [ "home" ] ->
      StaticPageRoute Home
    [ "help" ] ->
      StaticPageRoute Help
    [ "about" ] ->
      StaticPageRoute About
    [ "contact" ] ->
      StaticPageRoute Contact
    [ "user", "index" ] ->
      UserRoute Index
    [ "user", "show", id ] ->
      UserRoute ( Show ( idConvertStringToInt id ) )
    _ ->
      NotFound

idConvertStringToInt : String -> Int
idConvertStringToInt id =
  Result.withDefault 0 ( String.toInt id )

-- MODEL

type alias Model =
  { history : List Navigation.Location
  , currentRoute : Navigation.Location
  , user : UserSchema
  }

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
  ( Model [ location ] location initUser
  , Cmd.none
  )

-- UPDATE

type Msg
  = UrlChange Navigation.Location

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChange location ->
      let
        ( updateModel, cmd ) = routing location model
      in
        ( { updateModel | history = location :: model.history, currentRoute = location }
        , cmd
        )

-- VIEW

view : Model -> Html msg
view model =
  div []
    [ div [] [ ul []
               [ li [] [ text model.user.name ]
               , li [] [ text model.user.email ]
               , li [] [ text ( toString model.user.id ) ]
               ]
             ]
    , h1 [] [ text "Pages" ]
    , ul [] (List.map viewLink [ "home", "help", "about", "contact", "user/index", "user/show/1" ])
    , h1 [] [ text "History" ]
    , ul [] (List.map viewLocation model.history)
    , h1 [] [ text "CurrentRoute" ]
    , ul [] [ li [] [ text model.currentRoute.hash ] ]
    ]

viewLink : String -> Html msg
viewLink name =
  li [] [ a [ href ("#/" ++ name) ] [ text name ] ]

viewLocation : Navigation.Location -> Html msg
viewLocation location =
  li [] [ text (location.pathname ++ location.hash) ]

