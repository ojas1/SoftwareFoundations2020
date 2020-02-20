module Test.Generated.Main773111470 exposing (main)

import SystemTest
import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "SystemTest" [SystemTest.b1suite],     Test.describe "Example" [Example.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport Monochrome), seed = 33354855547523, processes = 4, paths = ["/home/ojas/iiith/courses/SoftwareFoundations/lf/SoftwareFoundations2020/reference-bs/tests/Example.elm","/home/ojas/iiith/courses/SoftwareFoundations/lf/SoftwareFoundations2020/reference-bs/tests/SystemTest.elm"]}