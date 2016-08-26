import Html
import Html.App as App
import List exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (value)

main =  App.beginnerProgram {
    model  = init,
    view   = view,
    update = update }

init : Model
init = { notes = [] , input_field = "", selected_note = Nothing, text_area = "" }

type Msg = AddNote | ChangeInputField String | SelectNote Id | ChangeDescription Id String

type alias Id = Int
type alias Title = String
type alias Content = String

type Note = Note Id Title Content

type alias Model = { notes : List Note, input_field : String, selected_note: Maybe Note, text_area : String }

getNoteById : Id -> List Note -> Maybe Note
getNoteById id list =
  let
    elem = head list
    list_tail = tail list
  in
    case elem of
       Just ((Note nid _ _) as note) ->
           if nid == id
           then Just note
           else
              case list_tail of
                Nothing -> Nothing
                Just l  -> getNoteById id l
       Nothing -> Nothing

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddNote                -> { model | notes = (Note (List.length model.notes) model.input_field "some desc") :: model.notes, input_field = "" }
    ChangeInputField a     -> { model | input_field = a }
    SelectNote id          -> { model | selected_note = (getNoteById id model.notes) }
    ChangeDescription id d -> { model | text_area = d }

view model =
  Html.div [] [
     Html.ul []
      (List.map (\note -> Html.li [] [renderNote note]) model.notes),
     Html.input [value model.input_field, onInput (\x -> ChangeInputField x) ] [],
     renderTextArea (model.selected_note),
     Html.button [ onClick AddNote ] [ Html.text "Add Note" ]
  ]

renderNote (Note id title content) = Html.div [] [
  Html.strong [ onClick (SelectNote id)] [Html.text title],
  Html.p [] [Html.text content]
 ]

renderTextArea  note  =
  case note of
     Just (Note id title content) -> Html.div [] [ Html.textarea [onInput (\x -> ChangeDescription x)] [Html.text content] ]
     Nothing                      -> Html.div [] []
