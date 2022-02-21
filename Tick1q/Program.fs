﻿open System

//------------------------write your answer function(s) here---------------------//

//oogabooga
//cool

// top-level subfunctions of polarToCartesianApprox (if any)
let fact x =
    if x = 0
    then 1.0
    else List.reduce (*) [1.0..x]

let removeZeroEl lst = 
    lst |> List.collect (fun x -> if x = 0.0 then [] else [x])

let elSign lst = 
    lst |> List.mapi (fun i x -> if i%2 = 0 then x else -x)

let termSine x n =
    let term i = 
        if i = 0 || i%2 = 0
        then 0.0
        else (x ** float i) / float(fact i)
    if n = 0
    then 0.0
    else [0..n] |> List.map term |> removeZeroEl |> elSign |> List.reduce(+)

let termCos x n =
    let term i =
        if i = 0
        then 1.0
        else if i%2 <> 0
        then 0.0
        else (x ** float i) / float(fact i)
    if n = 0
    then 1.0
    else [0..n] |> List.map term |> removeZeroEl |> elSign |> List.reduce(+)

/// answer to Tick1
// the header given here is correct.
let polarToCartesianApprox (r,theta) n = 
    let cosineTerm = termCos theta n
    let sineTerm = termSine theta n
    let x = r * cosineTerm
    let y = r * sineTerm
    (x,y)
        
    


//--------------------testbench code - DO NOT CHANGE-----------------------------//

/// used to make generate testbench data
let testInputs =
    let testPolarCoords = List.allPairs [1.;2.] [1.;2.]
    List.allPairs testPolarCoords [0;1;2;3;10]

/// data showing correct results generated with model answer and given here
let testBenchData =
    [
        ((1.0, 1.0), 0, (1.0, 0.0))       
        ((1.0, 2.0), 0, (1.0, 0.0))        
        ((2.0, 1.0), 0, (2.0, 0.0))        
        ((2.0, 2.0), 0, (2.0, 0.0))        
        ((1.0, 1.0), 1, (1.0, 1.0))        
        ((1.0, 2.0), 1, (1.0, 2.0))        
        ((2.0, 1.0), 1, (2.0, 2.0))        
        ((2.0, 2.0), 1, (2.0, 4.0))        
        ((1.0, 1.0), 2, (0.5, 1.0))        
        ((1.0, 2.0), 2, (-1.0, 2.0))        
        ((2.0, 1.0), 2, (1.0, 2.0))        
        ((2.0, 2.0), 2, (-2.0, 4.0))        
        ((1.0, 1.0), 3, (0.5, 0.8333333333))        
        ((1.0, 2.0), 3, (-1.0, 0.6666666667))        
        ((2.0, 1.0), 3, (1.0, 1.666666667))        
        ((2.0, 2.0), 3, (-2.0, 1.333333333))        
        ((1.0, 1.0), 10, (0.5403023038, 0.8414710097))        
        ((1.0, 2.0), 10, (-0.4161552028, 0.9093474427))        
        ((2.0, 1.0), 10, (1.080604608, 1.682942019))        
        ((2.0, 2.0), 10, (-0.8323104056, 1.818694885))
    ]
/// test testFun with testData to see whether actual results are the same as
/// expected results taken from testData
let testBench testData testFun =
    let closeTo f1 f2 = abs (f1 - f2) < 0.000001
    let testItem fn (coords, n, (expectedX,expectedY) as expected) =
        let actualX,actualY as actual = testFun coords n
        if not (closeTo actualX expectedX) || not (closeTo actualY expectedY) then
            printfn "Error: coords=%A, n=%d, expected result=%A, actual result=%A"coords n expected actual
            1
        else
            0
    printfn "Starting tests..."
    let numErrors = List.sumBy (testItem testFun) testData
    printfn "%d tests Passed %d tests failed." (testData.Length - numErrors) numErrors

[<EntryPoint>]
let main argv =
    testBench testBenchData polarToCartesianApprox
    0 // return an integer exit code
