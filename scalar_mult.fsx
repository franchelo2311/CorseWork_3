(* 
 * File: scalar_mult.fsx
 * Author: Franz Zbinden Garcia
 * Course: COTI 4039-KJ1
 * Date: 18/08/2024
 * Purpose: Computes the scalar multiplication of an integer and a list of integers using recursion, tail recursion, and higher-order functions.
 *)

// Multiplies a scalar by each element of a list using regular recursion.
let rec scalarMult num lst = // int -> int list -> int list
    match lst with
    | [] -> []
    | hd :: tl -> (hd * num) :: scalarMult num tl

// Multiplies a scalar by each element of a list using tail recursion for improved performance.
let scalarMultIter num lst = // int -> int list -> int list
    let rec loop num lst acc =
        match lst with
        | [] -> List.rev acc // Reverses the accumulated list after processing.
        | hd :: tl -> loop num tl ((num * hd) :: acc)
    loop num lst []

// Multiplies a scalar by each element of a list using higher-order functions for concise expression.
let scalarMultHigher num lst = // int -> int list -> int list
    lst |> List.map (fun x -> num * x)

// Example scalar and list to demonstrate the functions.
let scalar = 3
let numbers = [2; 7; 4]

// Display the results of each technique.
printfn "Computing the scalar multiplication of %A by %A" scalar numbers
printfn "Using regular recursion: %A" (scalarMult scalar numbers)
printfn "Using tail-recursive helper function: %A" (scalarMultIter scalar numbers)
printfn "Using List.map: %A" (scalarMultHigher scalar numbers)
