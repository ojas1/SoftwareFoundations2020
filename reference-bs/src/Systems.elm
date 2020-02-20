module Systems exposing (..)

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

type Msg = Select Int
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