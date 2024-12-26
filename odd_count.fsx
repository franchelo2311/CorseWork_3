(* 
 * File: fibonacci_fns.fsx
 * Author: Franz Zbinden Garcia
 * Course: COTI 4039-KJ1
 * Date: 18/08/2024
 * Purpose: Counts the number of odd numbers in a list using different techniques, including recursion, tail recursion, and higher-order functions.
 *)

// Counts the number of odd numbers in a list using regular recursion.
let rec OddCount lst = // int list -> int
    match lst with
    | [] -> 0
    | hd :: tl when hd % 2 <> 0 -> 1 + OddCount tl
    | _ :: tl -> OddCount tl

// Counts the number of odd numbers in a list using tail recursion for improved performance.
let OddCountIter lst = // int list -> int
    let rec loop lst acc =
        match lst with
        | [] -> acc
        | hd :: tl when hd % 2 <> 0 -> loop tl (acc + 1)
        | _ :: tl -> loop tl acc
    loop lst 0

// Counts the number of odd numbers in a list using higher-order functions for concise expression.
let OddCountHigher lst = // int list -> int
    lst
    |> List.filter (fun x -> x % 2 <> 0)
    |> List.length

// Example list of numbers to demonstrate the functions.
let numbers = [5; 10; 4; 3; 2; 9; 1; 6; -7; 8]

// Display the results of each technique.
printfn "Using regular recursion: %d" (OddCount numbers)
printfn "Using tail-recursive helper function: %d" (OddCountIter numbers)
printfn "Using List.filter and List.length: %d" (OddCountHigher numbers)
