module SystemTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Systems exposing (..)

b1suite : Test
b1suite =
    describe "The Bubblesort module"
        [ describe "selection functionality"
              [ test "select one item when one is already present" <|
                    \_ ->
                    Expect.equal (select 1 (OneSelected 3)) (BothSelected 3 1)
              , test "select one item when no item is selected" <|
                  \_ ->
                    Expect.equal (select 1 NoneSelected) (OneSelected 1)
              , test "select when both are selected" <|
                  \_ ->
                    Expect.equal (select 3 (BothSelected 4 5)) (BothSelected 4 5)
              , test "deselect one item when one is already present" <|
                    \_ ->
                    Expect.equal (deselect 3 (OneSelected 3)) NoneSelected
              , test "deselect one item when both items are selected" <|
                  \_ ->
                    Expect.equal (deselect 1 (BothSelected 1 2)) (OneSelected 2)
              , test "deselect one item (second) when both items are selected" <|
                  \_ ->
                    Expect.equal (deselect 2 (BothSelected 1 2)) (OneSelected 1)
              , test "deselect one item (the wrong one) when both items are selected" <|
                  \_ ->
                    Expect.equal (deselect 1 (BothSelected 5 2)) (BothSelected 5 2)
              , test "deselect when none is selected" <|
                  \_ ->
                    Expect.equal (deselect 1 NoneSelected) NoneSelected                        
              , test "isSelected when item is selected" <|
                  \_ ->
                      Expect.equal (isSelected 1 (OneSelected 1)) True
              , test "isSelected when item is selected (one of two)" <|
                  \_ ->
                      Expect.equal (isSelected 1 (BothSelected 1 4)) True
              , test "isSelected when item is selected (the second one)" <|
                  \_ ->
                      Expect.equal (isSelected 4 (BothSelected 1 4)) True
              , test "isSelected when not selected" <|
                  \_ ->
                      Expect.equal (isSelected 2 (BothSelected 1 4)) False
              , test "isSelected when not selected (in one)" <|
                  \_ ->
                      Expect.equal (isSelected 2 (OneSelected 1)) False
              , test "isSelected when not selected (Noneselected)" <|
                  \_ ->
                      Expect.equal (isSelected 2 NoneSelected) False
              ]
        , describe "swapping"
            [ test "both selected" <|
                  \_ -> Expect.equal (swap [10, 40, 50, 15] (BothSelected 0 3)) [15, 40, 50, 10]
            , test "one selected" <|
                  \_ -> Expect.equal (swap [10, 40, 50, 15] (OneSelected 2)) [10, 40, 50, 15]
            ]
        , describe "ordering"
            [ test "both selected" <|
                  \_ -> Expect.equal (order [4, 7, 5, 9] (BothSelected 1 2)) [4, 5, 7, 9]
            , test "both selected, no order" <|
                  \_ -> Expect.equal (order [4, 5, 7, 9] (BothSelected 1 2)) [4, 5, 7, 9]
            ]
        , describe "adjacent ordering"
            [ test "both selected" <|
                  \_ -> Expect.equal (adjOrder [4, 7, 5, 9] (Just 1)) [4, 5, 7, 9]
            , test "not selected" <|
                  \_ -> Expect.equal (adjOrder [3, 6, 2] Nothing) [3, 6, 2]
            ]
        , describe "increment"
            [ test "un-ordered" <|
                  \_ -> Expect.equal (increment [2, 5, 1] 1) ([2, 1, 5], 2)
            , test "out of range index" <|
                  \_ -> Expect.equal (increment [1, 3] 1) ([1, 3], 1)
            ]
        , describe "reset"
            [ test "reset out of range" <|
                  \_ -> Expect.equal (reset [2] 4) ([2], 0)
            ]
        ]
