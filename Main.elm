module Main exposing (..)

import Html
import Html.App as App
import List exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (value, class, placeholder, href, for)
import Html.Attributes as HA
import Date exposing (..)
import Task
import Time exposing (Time)

import String exposing (..)

main =  App.program {
    init  = init,
    view   = view,
    update = update,
    subscriptions = (\_ -> Sub.none)
  }

init : (Model, Cmd Msg)
init = ({ notes = [] , selected_note = Nothing, title_input = "", content_input = ""  }, Cmd.none)

type Msg = AddNote Time | SelectNote Id | ChangeContent Id String | ChangeTitle Id String | GetTimeAndThen (Time -> Msg)

type alias Id = Int
type alias Title = String
type alias Content = String

type Note = Note Id Title Content Time
type alias Notes = List Note

type alias Model = {
  selected_note: Maybe Note,
  notes : Notes,
  title_input : String,
  content_input : String
}


getDate : Time -> String
getDate time =
  let
    d = fromTime time
  in
    toString (day d) ++ " " ++ toString (month d) ++ " " ++ toString (year d)

getNoteById : Id -> Notes -> Maybe Note
getNoteById id list =
  let
    elem = head list
    list_tail = tail list
  in
    case elem of
       Just ((Note nid _ _ _) as note) ->
           if nid == id
           then Just note
           else
              case list_tail of
                Nothing -> Nothing
                Just l  -> getNoteById id l
       Nothing -> Nothing

changeContentByNoteId : Id -> Content -> List Note -> List Note
changeContentByNoteId id content list =
  List.map (\((Note nid ntitle ncontent ndate) as note) ->
             if nid == id
             then (Note nid ntitle content ndate)
             else note) list

changeTitleByNoteId : Id -> Title -> List Note -> List Note
changeTitleByNoteId id title list =
  List.map (\((Note nid ntitle ncontent ndate) as note) ->
             if nid == id
             then (Note nid title ncontent ndate)
             else note) list


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetTimeAndThen successHandler -> (model, (Task.perform assertNeverHandler successHandler Time.now))
    AddNote             time ->
      let
        new_note = (Note (List.length model.notes) "" "" time)
      in
        ({ model | notes = new_note :: model.notes, selected_note = Just new_note, content_input = "", title_input = "" }, Cmd.none)
    SelectNote id            ->
       let
         note = (getNoteById id model.notes)
       in
         case note of
           Nothing           -> ({ model | selected_note = (Debug.log "selected note: " Nothing)  }, Cmd.none)
           Just (Note i t d _) -> ({ model | selected_note = (Debug.log "selected note: " note), content_input = (Debug.log "content_input: " d), title_input = t }, Cmd.none)
    ChangeContent id c       -> ({ model | notes = changeContentByNoteId id c model.notes, content_input = c }, Cmd.none)
    ChangeTitle id t         -> ({ model | notes = changeTitleByNoteId id t model.notes, title_input = t }, Cmd.none)


addNote = GetTimeAndThen (\time -> AddNote time)

assertNeverHandler : a -> b
assertNeverHandler =
    (\_ -> Debug.crash "This should never happen")


view model =
  Html.div [ class "row container" ] [
     Html.div [ class "col-md-4"] [
        Html.h1 [] [ Html.text "My awesome notes"],
        Html.div [ class "list-group" ]
          (List.map (\((Note id _ _ _) as note) -> Html.a [ onClick (SelectNote id), href "#", class "list-group-item list-group-item-action"] [renderNote note]) model.notes) ,
        Html.button [ class "btn btn-primary", onClick addNote ] [ Html.text "Add new" ]
     ],
     Html.div [ class "col-md-8"] [
        renderForm model
     ]
  ]


renderNote (Note id title content time) = Html.div [] [
  Html.h5 [ class "list-group-item-heading"] [Html.text title],
  Html.p [ class "list-group-item-text"] [Html.text ((getDate time) ++ " | " ++ String.left 100 content)]
 ]


renderForm model =
    case model.selected_note of
      Nothing -> Html.div [ class "form-group" ] []
      Just (Note id title content _) ->
        Html.div [ class "form-group"] [
          Html.label [ for "title-input" ] [ Html.text "Title" ],
          Html.input [ HA.id "title-input", class "form-control", value model.title_input, onInput (\x -> ChangeTitle id x) ] [],
          Html.label [ for "content-input" ] [ Html.text "Content" ],
          Html.textarea [HA.id "content", class "form-control", value model.content_input, onInput (\x -> ChangeContent id x)] [Html.text content]
        ]
