module CandyCrush exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time
import Random
import Html.Events.Extra.Pointer exposing (..)


main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

type CandyType = Red
               | Blue
               | Green
               | Yellow
               | Purple

type alias Piece = {x:Int
                   ,y:Int
                   ,color:Int
                   ,matching:Bool
                   ,moving:Bool
                   ,current: Position
                   ,start: Position
                   }

type State = Waiting
           | Moving Piece
           | Matching
           | Deleting
           | Dropping
           | Adding

type alias Position = {x : Float, y : Float}

type alias Model = {conf:List Piece
                   ,state : State
                   ,elapsed: Int
                   }

type Msg = Roll
         | Generated (List Int)
         | MoveStart Piece {x:Float, y:Float}
         | MoveEnd {x:Float, y:Float}
         | Move {x:Float, y:Float}
         | Elapsed Time.Posix
         | Added (List Int)

hSize : Int
hSize = 5
vSize : Int
vSize = 5

init: () -> (Model, Cmd Msg)
init _ = ({conf = List.map newPiece (List.range 0 (hSize*vSize-1))
          ,state = Waiting
          ,elapsed = 10
          }
         ,Random.generate Generated (Random.list 25 (Random.int 0 4)))

newPiece: Int -> Piece
newPiece i =
    {x=(modBy hSize i)
    ,y=(i // hSize)
    ,color = 0
    ,matching = False
    ,moving = False
    ,current = Position 0 0
    ,start = Position 0 0
    }

hMatchAt: Piece -> Model -> List Piece
hMatchAt p model =
    let
        right = List.filter (\q -> q.color == p.color &&
                         q.y == p.y &&
                         q.x - p.x <= 2 &&
                         q.x >= p.x ) model.conf
        others = List.filter (\q -> not(q.color == p.color &&
                         q.y == p.y &&
                         q.x - p.x <= 2 &&
                         q.x >= p.x) ) model.conf
    in
       if (List.length right) >= 3 then
           List.concat
               [(List.map (\q -> {q | matching =True }) right)
               ,others]
       else
           model.conf

vMatchAt: Piece -> Model -> List Piece
vMatchAt p model =
    let
        below = List.filter (\q -> q.color == p.color &&
                           q.x == p.x &&
                           q.y - p.y <= 2 &&
                           q.y >= p.y ) model.conf
        others = List.filter (\q -> not(q.color == p.color &&
                           q.x == p.x &&
                           q.y - p.y <= 2 &&
                           q.y >= p.y) ) model.conf
    in
        if (List.length below) >= 3 then
            List.concat
               [(List.map (\q -> {q | matching =True }) below)
               ,others]
        else
            model.conf

match: Model -> Model
match model =
    let
        newconf = List.foldl (\p conf -> vMatchAt p {model | conf = conf}) model.conf model.conf
        newnewconf = List.foldl (\p conf -> hMatchAt p {model | conf = conf}) newconf newconf
        isMatched = List.any (\p -> p.matching) newnewconf
    in
        if isMatched then
            {model | conf = newnewconf ,state=Matching}
        else
            {model| state = Waiting}

deleteMatch: Model -> Model
deleteMatch model =
    {model | conf = List.filter(\p -> not p.matching) model.conf
    ,state=Deleting
    ,elapsed=0 }

dropColumn: List Piece -> List Piece
dropColumn conf =
    List.sortBy .y conf |> List.indexedMap (\i p -> {p| y=i})

drop: Model -> Model
drop model =
    let
        conf = List.concat <| List.map(\j ->drop1 j model) (List.range 0 4)
    in
        {model | conf=conf
        ,state = Adding
        ,elapsed = 0}


drop1: Int -> Model -> List Piece
drop1  j model =

       dropColumn (List.filter (\p -> p.x==j) model.conf)

add: List Int -> Model -> Model
add rList model =
    let
        conf=model.conf
        newconf = List.concat <|
                  List.map
                  (\j -> add1 j (List.take 5 <| List.drop (5*j) rList) model)
                  (List.range 0 4)
    in
        {model | conf = newconf
        --,state = Matching}
        ,state = Waiting}

add1: Int -> List Int -> Model -> List Piece
add1 j rList model =
    let
        column = List.filter (\p -> p.x==j) model.conf
        colors = List.take (5-(List.length column)) rList
        addend = List.indexedMap
                 (\i c -> {x = j
                          ,y=((List.length column) +i)
                          ,color = c
                          ,matching = False
                          ,moving = False
                          ,current = Position 0 0
                          ,start = Position 0 0
                          })
                 colors
    in
        List.concat [column,addend]



swap: Piece -> Model -> Model
swap end model =
    case model.state of
        Moving start ->
            let
                others = List.filter(\p -> ((p.x/=end.x) || (p.y/=end.y)) && ((p.x/=start.x) || (p.y/=start.y))) model.conf
                distance = (abs (start.x - end.x))+(abs (start.y - end.y))
            in
                if distance /= 1 then
                    {model | state = Waiting
                    , conf = List.map (\p -> {p|moving=False}) model.conf}
                else
                    let
                        swapped = match <|
                                  {model | conf = List.concat[
                                                   [{start|color =end.color
                                                    ,start=Position 0 0
                                                    ,current=Position 0 0
                                                    }
                                                   ,{end|color=start.color
                                                    ,start=Position 0 0
                                                    ,current=Position 0 0
                                                    }]
                                                  ,others]
                                  ,elapsed = 0}
                        existMatching = List.foldl (\p matching -> p.matching || matching) False swapped.conf
                    in
                        if existMatching then
                            swapped
                        else
                            {model | state = Waiting
                            , conf = List.map (\p -> {p|moving=False}) model.conf}
        _ -> model

update msg model =
    case msg of
        Generated rlist ->( match<| {model|conf=randomize model.conf rlist}, Cmd.none)
        MoveStart piece pos ->
            let
                moving = List.map (\p -> {p|moving=True
                                         ,start=pos
                                         ,current=pos
                                         }
                                  ) <|
                         List.filter (\p -> piece.x == p.x && piece.y == p.y) model.conf
                others = List.filter (\p -> piece.x /= p.x || piece.y /= p.y) model.conf
                conf = List.concat [others, moving]
            in
                ({model | state = (Moving piece)
                 ,conf = conf
                 },Cmd.none)
        MoveEnd pos ->
            let
                newx = floor (pos.x / (toFloat unit))
                newy = hSize - floor (pos.y / (toFloat unit))
                dest = List.head <| List.filter (\p -> p.x==newx && p.y==newy) model.conf
            in
                case dest of
                    Just piece -> (match <|swap piece model , Cmd.none)
                    _ -> (model , Cmd.none)
        Added rlist -> (add rlist model,Cmd.none)
        Move pos ->
            ({model | conf = List.map (\p -> if p.moving then
                                                 {p | current = pos}
                                             else
                                                 p
                                      ) model.conf
             }, Cmd.none)
        Elapsed t ->
            case model.state of
                Waiting ->
                    (match model, Cmd.none)
                Matching ->
                    if model.elapsed > 1 then
                        (deleteMatch model,Cmd.none)
                    else
                        ({model | elapsed = (model.elapsed+1)},Cmd.none)
                Deleting ->
                    if model.elapsed > 1 then
                        (drop model
                        ,Random.generate Added (Random.list 25 (Random.int 0 4)))
                    else
                        ({model | elapsed = (model.elapsed+1)},Cmd.none)
                _ -> ({model | elapsed = (model.elapsed+1)},Cmd.none)
        _ ->(model,Cmd.none)

randomize conf rlist =
    List.indexedMap (\idx p -> {p | color = Maybe.withDefault 0
                                    (List.head (List.drop idx rlist))}) conf

pallet colornum =
    case colornum of
        0 -> "violet" --grape
        1 -> "yellow" --banana
        2 -> "skyblue" --soda
        3 -> "lightgreen" --melon
        4 -> "orange" -- orange
        _ -> "black"

unit = 100
pieceView: Piece -> Svg Msg
pieceView pdata =
    let
        xpixel = if pdata.moving then
                    (toFloat <| pdata.x*unit+100)+pdata.current.x-pdata.start.x
                else
                    toFloat (pdata.x*unit+100)
        ypixel = if pdata.moving then
                    (toFloat <| hSize*unit-pdata.y*unit+100)+pdata.current.y-pdata.start.y
                else
                    toFloat <| hSize*unit-pdata.y*unit+100
        shape color =
          case color of
           0 -> g[][circle [cx (String.fromInt (unit//2)) --grape1
                ,cy (String.fromInt (3*unit//4))
                ,r (String.fromInt (unit//7))
                ,fill ( pallet color)
                ][]
                ,circle [cx (String.fromInt (5*unit//14)) --grape2
                        ,cy (String.fromInt (unit//2))
                        ,r (String.fromInt (unit//7))
                        ,fill ( pallet color)
                        ]
                []
                ,circle [cx (String.fromInt (9*unit//14))--3
                        ,cy (String.fromInt (unit//2))
                        ,r (String.fromInt (unit//7))
                        ,fill ( pallet color)
                        ]
                []
                ,circle [cx (String.fromInt (3*unit//14))--4
                        ,cy (String.fromInt (unit//4))
                        ,r (String.fromInt (unit//7))
                        ,fill ( pallet color)
                        ]
                []
                ,circle [cx (String.fromInt (unit//2))--5
                        ,cy (String.fromInt (unit//4))
                        ,r (String.fromInt (unit//7))
                        ,fill ( pallet color)
                        ,transform "translate(-0.5, 0)"
                        ]
                []
                ,circle [cx (String.fromInt (11*unit//14)) --6
                        ,cy (String.fromInt (unit//4))
                        ,r (String.fromInt (unit//7))
                        ,fill ( pallet color)
                        ]
                []
                ,circle [cx (String.fromInt (13*unit//28))
                        ,cy (String.fromInt (5*unit//7))
                        ,r (String.fromInt (unit//25))
                        ,fill "white"
                        ][]
                ,circle [cx (String.fromInt (9*unit//28))
                        ,cy (String.fromInt (13*unit//28))
                        ,r (String.fromInt (unit//25))
                        ,fill "white"
                        ][]
                ,circle [cx (String.fromInt (17*unit//28))
                        ,cy (String.fromInt (13*unit//28))
                        ,r (String.fromInt (unit//25))
                        ,fill "white"
                        ][]
                ,circle [cx (String.fromInt (5*unit//28))
                        ,cy (String.fromInt (3*unit//14))
                        ,r (String.fromInt (unit//25))
                        ,fill "white"
                        ][]
                ,circle [cx (String.fromInt (13*unit//28))
                        ,cy (String.fromInt (3*unit//14))
                        ,r (String.fromInt (unit//25))
                        ,fill "white"
                        ,transform "translate(-0.5, 0)"
                        ][]
                ,circle [cx (String.fromInt (3*unit//4))
                        ,cy (String.fromInt (3*unit//14))
                        ,r (String.fromInt (unit//25))
                        ,fill "white"
                        ][]
             ]

           1 -> g[][Svg.path [d "M 0 0 c 70 20, 70 80, 0 100 c 50 -30, 50 -70, 0 -100 z"
                             ,fill "gold"
                             ,transform "scale(0.8, 0.8), translate(90, 20), rotate(45)"
                             ][]
                    ,Svg.path [d "M 0 0 c 50 30, 50 70, 0 100 c 30 -40, 30 -60, 0 -100 z"
                              ,fill ( pallet color)
                              ,transform "scale(0.8, 0.8), translate(90, 20), rotate(45)"
                              ][]
                    ,rect [x (String.fromInt (2*unit//3))
                          ,y (String.fromInt (2*unit//15))
                          ,width (String.fromInt (unit//6))
                          ,height (String.fromInt (unit//15))
                          ,fill ( pallet color)
                          ][]
             ]

           2 -> g[][circle [cx (String.fromInt (unit//2))
                            ,cy (String.fromInt (unit//2))
                            ,r (String.fromInt (unit//3))
                            ,fill ( pallet color)
                            ][]
                   ,ellipse [cx (String.fromInt (unit//2))
                            ,cy (String.fromInt (3*unit//7))
                            ,rx (String.fromInt (4*unit//11))
                            ,ry (String.fromInt (unit//30))
                            ,fill ( pallet color)
                            ][]
                    ,rect [x (String.fromInt (3*unit//22))
                          ,y (String.fromInt (3*unit//7))
                          ,width (String.fromInt (8*unit//11))
                          ,height (String.fromInt (unit//7))
                          ,fill ( pallet color)
                          ,transform "translate(1, 0.5)"
                          ][]
                   ,ellipse [cx (String.fromInt (unit//2))
                            ,cy (String.fromInt (4*unit//7))
                            ,rx (String.fromInt (4*unit//11))
                            ,ry (String.fromInt (unit//30))
                            ,fill ( pallet color)
                            ][]
                   ,circle [cx (String.fromInt (4*unit//10))
                           ,cy (String.fromInt (4*unit//10))
                           ,r (String.fromInt (unit//10))
                           ,fill "white"
                           ][]
             ]

           3 -> g[][rect [x (String.fromInt (unit//3))
                         ,y (String.fromInt (2*unit//15))
                         ,width (String.fromInt (unit//3))
                         ,height (String.fromInt (unit//15))
                         ,fill ( pallet color)
                         ][]
                   ,rect [x (String.fromInt (9*unit//20))
                         ,y (String.fromInt (2*unit//15))
                         ,width (String.fromInt (unit//15))
                         ,height (String.fromInt (unit//5))
                         ,fill ( pallet color)
                         ][]
                   ,circle [cx (String.fromInt (unit//2))
                            ,cy (String.fromInt (3*unit//5))
                            ,r (String.fromInt (unit//3))
                            ,fill ( pallet color)
                            ][]
                    ,circle [cx (String.fromInt (4*unit//10))
                            ,cy (String.fromInt (unit//2))
                            ,r (String.fromInt (unit//10))
                            ,fill "white"
                            ][]
                    ,Svg.path [d "M 40 40 L 70 70"
                              ][]
             ]

           4 ->  g[][circle [cx (String.fromInt (unit//2))
                            ,cy (String.fromInt (unit//2))
                            ,r (String.fromInt (unit//3))
                            ,fill ( pallet color)
                            ][]
                    ,circle [cx (String.fromInt (4*unit//10))
                            ,cy (String.fromInt (4*unit//10))
                            ,r (String.fromInt (unit//10))
                            ,fill "white"
                            ][]
                    ,ellipse [cx (String.fromInt (unit//2))
                             ,cy (String.fromInt (unit//4))
                             ,rx (String.fromInt (unit//15))
                             ,ry (String.fromInt (unit//32))
                             ,fill "green"
                             ][]
            ]

           _ -> g[][circle [cx (String.fromInt (unit//2))
                            ,cy (String.fromInt (unit//2))
                            ,r (String.fromInt (unit//3))
                            ,fill ( pallet color)
                            ][]
                    ,circle [cx (String.fromInt (4*unit//10))
                            ,cy (String.fromInt (4*unit//10))
                            ,r (String.fromInt (unit//10))
                            ,fill "white"
                            ][]
                   ]
    in
    g [transform ("translate(" ++ (String.fromFloat xpixel)
                      ++ "," ++ (String.fromFloat ypixel) ++ ")")
      ,onDown (relativePos >> (MoveStart pdata))
      ,onMove (relativePos >> Move)
      ,onUp (relativePos >> MoveEnd)
      ]
    [rect [x "0"
          ,y "0"
          ,width (String.fromInt unit)
          ,height (String.fromInt unit)
          ,stroke "white"
          ,strokeWidth "4px"
          ,fill (if pdata.matching then
                        "red"
                    else
                        "#aaa")
          ]
         [
         ]
    ,(shape pdata.color)
    ]

relativePos : Event -> {x:Float, y:Float}
relativePos event =
    {x=(Tuple.first event.pointer.offsetPos)-100
    ,y=(Tuple.second event.pointer.offsetPos)-100
    }

view model =
    Html.div [Html.Attributes.style "touch-action" "none"]
        [svg [width "800"
             ,height "800"
             ]
             (List.map pieceView model.conf)
        ]

subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 100 Elapsed
