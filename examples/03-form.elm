import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Char exposing (isDigit, isUpper, isLower)
import String exposing (all, any, length)
import List exposing (isEmpty, member)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , isSubmitting : Bool
  }

model : Model
model =
  Model "" "" "" "" False


-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name, isSubmitting = False }

    Password password ->
      { model | password = password, isSubmitting = False }

    PasswordAgain password ->
      { model | passwordAgain = password, isSubmitting = False }
    
    Age age ->
      { model | age = age, isSubmitting = False }

    Submit ->
      { model | isSubmitting = True }


-- VIEW

view : Model -> Html Msg
view model =
  div
    []
    ( List.filterMap identity
      [ Just <| input [ type_ "text", placeholder "Name", onInput Name ] []
      , Just <| input [ type_ "password", placeholder "Password", onInput Password ] []
      , Just <| input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
      , Just <| input [ type_ "text", placeholder "Age", onInput Age ] []
      , Just <| button [ onClick Submit ] [ text "Submit" ]
      , maybeIf model.isSubmitting (viewValidation model)
      ] )

viewValidation : Model -> Html msg
viewValidation model =
  let
    validationFailures = 
      List.concat
        [ passwordsValidationFailures model.password model.passwordAgain
        , validateRules model.age ageRules
        ]
  in
    let
      (color, message) =
        if not (isEmpty validationFailures) then
          ("red", String.join " " validationFailures)
        else
          ("green", "OK")
    in
      div [ style [("color", color)] ] [ text message ]

type alias ValidationRule =
  { predicate : String -> Bool
  , description : String
  }

passwordRules : List ValidationRule
passwordRules =
  [ ValidationRule (\p -> length(p) > 7) "Password must contain at least 8 characters."
  , ValidationRule (any isDigit) "Password must contain at least one digit."
  , ValidationRule (any isUpper) "Password must contain at least one uppercase letter."
  , ValidationRule (any isLower) "Password must contain at least one lowercase letter."
  ]

ageRules : List ValidationRule
ageRules =
  [ ValidationRule (all isDigit) "Age must contain only digits."
  ]

passwordsValidationFailures : String -> String -> List String
passwordsValidationFailures password passwordAgain =
  List.concat
    [ maybeToList (passwordsMustMatch password passwordAgain)
    , listIntersect
        (validateRules password passwordRules)
        (validateRules passwordAgain passwordRules)
    ]

passwordsMustMatch : String -> String -> Maybe String
passwordsMustMatch password passwordAgain =
  maybeIf (password /= passwordAgain) "Passwords must match."

validateRules : String -> List ValidationRule -> List String
validateRules =
  List.filterMap << validateRule

validateRule : String -> ValidationRule -> Maybe String
validateRule value { predicate, description } =
  maybeIf (not <| predicate value) description

maybeIf : Bool -> a -> Maybe a
maybeIf test value =
  if test then
    Just value
  else
    Nothing

maybeToList : Maybe a -> List a
maybeToList maybe =
  case maybe of
    Nothing -> []
    Just value -> [ value ]

singletonIf : Bool -> a -> List a
singletonIf =
  maybeToList <<. maybeIf

listIntersect : List a -> List a -> List a
listIntersect first second =
  List.filter ((flip member) second) first

(<<.) : (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(<<.) = (<<)<<(<<)

(<<..) : (b -> c) -> (a -> a1 -> a2 -> b) -> a -> a1 -> a2 -> c
(<<..) = (<<)<<(<<.)

