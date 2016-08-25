import Html
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (value)

main =  App.beginnerProgram {
    model  = init,
    view   = view,
    update = update }

init : Model
init = { notes = [] , input_note = "" }

type Msg = AddNote | ChangeInputNote String

type alias Title = String
type alias Content = String

type Note = Note Title Content

type alias Model = { notes : List Note, input_note : String }

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddNote           -> { model | notes = (Note model.input_note "some desc") :: model.notes, input_note = "" }
    ChangeInputNote a -> { model | input_note = a }

view model =
  Html.div [] [
     Html.ul []
      (List.map (\note -> Html.li [] [renderNote note]) model.notes),
     Html.input [value model.input_note, onInput (\x -> ChangeInputNote x) ] [],
     Html.button [ onClick AddNote ] [ Html.text "Add Note" ]
  ]

renderNote (Note title content) = Html.div [] [
  Html.strong [] [Html.text title],
  Html.p [] [Html.text content]
 ]
