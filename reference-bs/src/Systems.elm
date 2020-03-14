module Systems exposing (..)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Browser
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import List.Extra as Extra exposing (swapAt, getAt)

-- B1

-- TYPES
type Selections
    = NoneSelected
    | OneSelected Int
    | BothSelected Int Int

      
{- define the data structures -}
type alias Model =
    { array : List Int
    , selections : Selections
    }

    
{- initialize the data structures -}
init_b1 =
    { array = [30, 24, 56, 5, 0]
    , selections = (BothSelected 0 2)
    }

type Msg = ArrayItemClicked Int
         | Swap


-- functions defined on the data structure

select : Int -> Selections -> Selections
select i selections =
    case selections of
        NoneSelected ->
            OneSelected i
        OneSelected j ->
            BothSelected j i
        BothSelected j k ->
            BothSelected j k


deselect : Int -> Selections -> Selections
deselect i selections =
    case selections of
        NoneSelected ->
            NoneSelected
        OneSelected j ->
            if (i == j) then 
                NoneSelected
            else
                OneSelected j
        BothSelected j k ->
            if (i == j) then
                OneSelected k
            else
                if (i == k) then
                    OneSelected j
                else (BothSelected j k)
                
                
isSelected : Int -> Selections -> Bool
isSelected index selections =
    case selections of
        NoneSelected ->
            False
        OneSelected j ->
            if j == index then True else False
        BothSelected j k ->
            if (j == index || k == index) then True else False

                
swap : List Int -> Selections -> List Int
swap array selections =
    case selections of
        NoneSelected -> array
        OneSelected i -> array
        BothSelected i j -> swapAt i j array

-- B2

outOfOrder i j x y = 
    let c1 = ( (i > j) && (x < y) )
        c2 = ( (i < j) && (x > y) )
    in
        (c1 || c2)                    

order : List Int -> Selections -> List Int
order array selections =
    case selections of
        BothSelected i j ->
            let ai = getAt i array
                aj = getAt j array
            in
                case (ai, aj) of
                    (Just x, Just y) ->
                        if (outOfOrder i j x y) then
                                swap array selections
                            else
                                array
                    _ -> array
        _ -> array

-- B3

adjOrder : List Int -> Maybe Int -> List Int
adjOrder array selection =
    case selection of
        Just i ->
                order array (BothSelected i (i + 1))
        Nothing -> array

-- B4

init_b4 =
    { array = [10, 45, 23, 56]
    , selection = Just 3
    }

increment : List Int -> Int -> (List Int, Int)
increment array index =
    if (index < (List.length array) - 1) then
        (adjOrder array (Just index), (index + 1))
    else
        (array, index)

reset : List Int -> Int -> (List Int, Int)
reset array index = (array, 0)


update: Msg -> Model -> Model
update msg model =
    let {array, selections} = model
    in
        case msg of
            ArrayItemClicked idx ->
                if (isSelected idx selections) then
                    { model | selections = deselect idx selections }
                else
                    { model | selections = select idx selections }
            Swap ->
                {model | array = swap array selections}

        
view: Model -> Html Msg
view model =
    let {array, selections} = model
    in
        Html.div
            [ HA.style "border" "1px solid black"
            , HA.style "margin" "auto"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "width" "max-content"
            ]
            [
             Svg.svg
                 [ SA.width "500px"
                 , SA.height "500px"
                 , SA.viewBox "0 0 500 500"
                 ]
                 [ viewArray array selections
                 ]
            , viewControls selections                 
            ]

            
viewArray: List Int -> Selections -> Svg Msg
viewArray array selections =
    let arrayYPos = 250.0
        arrayXPos = 50.0
        itemRadius = 30.0                     
    in
        Svg.g
            []
            (List.indexedMap (\i a ->
                                  Svg.g
                                  [SE.onClick (ArrayItemClicked i)]
                                  [ Svg.circle
                                        [ SA.cx (String.fromFloat (arrayXPos + (toFloat i) * 2.5 * itemRadius))
                                        , SA.cy (String.fromFloat arrayYPos)
                                        , SA.r (String.fromFloat itemRadius)
                                        , SA.fill (if (isSelected i selections) then "rgb(200, 200, 200)" else "rgb(100, 100, 100)")
                                        ]
                                        []
                                  , Svg.text_
                                      [ SA.x (String.fromFloat (arrayXPos + (toFloat i) * 2.5 * itemRadius))
                                      , SA.y (String.fromFloat arrayYPos)
                                      , SA.fontSize "30"
                                      , SA.fontWeight "bold"
                                      , SA.fill "rgb(0,0,0)"
                                      , SA.textAnchor "middle"
                                      , SA.dominantBaseline "middle"
                                      ]
                                        [a |> String.fromInt |> Svg.text]
                                  ]
                             ) array
            )


viewControls : Selections -> Html Msg
viewControls selections =
    case selections of
        NoneSelected ->
            Html.text ""
        OneSelected i ->
            Html.text ""
        BothSelected i j ->
            Html.span[HE.onClick Swap][Html.text "swap"]
            

main = Browser.sandbox
       { init = init_b1
       , update = update
       , view = view
       }
