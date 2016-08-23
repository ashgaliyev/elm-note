import Html
import Html.App as App
import Html.Events exposing (..)

main =  App.beginnerProgram {
    model  = init,
    view   = view,
    update = update }

init : Model
init = { notes = [] } 

type Msg = AddNote | DeleteLast 

type alias Model = { notes : List String } 

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddNote -> { notes = "Some note" :: model.notes  }
    _       -> { notes = model.notes  } 

view model = 
  Html.div [] [
     Html.ul [] (List.map (\x -> Html.li [] [Html.text x]) model.notes),
     Html.button [ onClick AddNote ] [ Html.text "Add Note" ]  
  ]

