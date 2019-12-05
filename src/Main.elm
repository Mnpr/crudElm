module Main exposing(main)

import Browser
import Html exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Json.Decode as Json

-------- Application States -------

type alias Todo = 
    {
        lid : Int
        , listItem : String
        , itemsList : List ListItem
    }

type alias ListItem = 
    {
        id : Int
        , isComplete : Bool
        , edit : Bool
        , info : String
    }

--------- Initial States ---------

initState : Todo
initState = 
    {
        lid = 0
        , listItem = ""
        , itemsList = []  
    }

-- init : Maybe Todo -> Todo 
-- init maybeTodo =
--     Maybe.withDefault initState maybeTodo

-------------- Local Storage

-- port setStorage : Todo -> Cmd msg

-- updateWithStorage : Msg -> Todo -> ( Todo, Cmd Msg )
-- updateWithStorage msg todoModel =
--     let
--         ( newModel, cmds ) =
--             update msg todoModel
--     in
--         ( newModel
--         , Cmd.batch [ setStorage newModel, cmds ]
--         )

----------- Update -----------

type Msg =
        NoOp
        | Create
        | UpdateListItem String
        | UpdateItemsList Int String
        | Check Int Bool
        | Remove Int

update : Msg -> Todo -> Todo
update msg todo =
    case msg of
        NoOp ->
            todo
            -- (todo, Cmd.none)
        Create ->
            {   todo 
                | lid = todo.lid + 1
                , listItem = ""
                , itemsList = todo.itemsList ++ [ newTodo todo.listItem todo.lid ] 
            }
            -- ({   todo 
            --     | lid = todo.lid + 1
            --     , listItem = ""
            --     , itemsList = todo.itemsList ++ [ newTodo todo.listItem todo.lid ] 
            -- }, Cmd.none)
        -----------------------------------------
        UpdateListItem  newItem ->
                    {   todo | listItem = newItem }
            -- ({   todo | listItem = newItem }, Cmd.none)
            
        UpdateItemsList id task ->
            let
                updateListItem t =
                    if t.id == id then
                        { t | info = task }
                    else
                        t
            in
                { todo | itemsList = List.map updateListItem todo.itemsList }
                -- ({ todo | itemsList = List.map updateListItem todo.itemsList }, Cmd.none)
        ------------------------------------------
        Check id checkItem ->
                let
                    completeItem t =
                        if t.id == id then 
                            { t | isComplete = checkItem }
                        else 
                            t
                in 
                    { todo | itemsList = List.map completeItem todo.itemsList }
                    -- ({ todo | itemsList = List.map completeItem todo.itemsList }, Cmd.none)
        -------------------------------------------    
        Remove id ->
            {
                todo | itemsList = List.filter (\x -> x.id /= id ) todo.itemsList  
            }
            -- ({
            --     todo | itemsList = List.filter (\x -> x.id /= id ) todo.itemsList  
            -- }, Cmd.none)

newTodo : String -> Int -> ListItem
newTodo  todoInfo id =
    {
        id = id
        , isComplete = False
        , edit = False
        , info = todoInfo
    }


-------------- View -------------

--------- Parent View

view : Todo -> Html Msg
view todo = 
    div []
        [ h1 [class "heading"] [text "Todo list"]
        , div [ class "parentView"
            ]
            [ lazy viewInput todo.listItem
            , lazy viewItemsList todo.itemsList
            ]
        ]

--------- View Input Text and add button

viewInput : String -> Html Msg
viewInput list =
    div [][
        input [ class "todoInput"
        , placeholder "things to do ..."
        , value list, name "todoInput" 
        , onInput UpdateListItem
        , onEnter Create][]  
    ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

--------- View Individual listitems

viewlistItem :ListItem -> Html Msg
viewlistItem todo =
    li
        [classList [("completed", todo.isComplete)] ]
        [ input [class "checkTodo"
                    , type_ "checkbox"
                    , checked todo.isComplete
                    , onClick (Check todo.id (not todo.isComplete))
                    ] []
        , input
            [ class "updateTodo"
            , value todo.info
            , name "title"
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput (UpdateItemsList todo.id)
            ][]
        , button [class "removeTodo", onClick (Remove todo.id)] [text "X"]
        ]

--------- View itemsList

viewItemsList : List ListItem -> Html Msg 
viewItemsList todoList=
    ul[class "viewList"]
        <| List.map viewlistItem todoList



-----------------------------------------------------------------------------
-- main : Program (Maybe Todo) Todo Msg
main =
    Browser.sandbox        
        { init = initState
        , view = view
        -- , view = \todo -> { title = "Todo", body = [ view todo ] } 
        , update = update
        -- , subscriptions = \_ -> Sub.none
        }
