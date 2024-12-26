(* 
 * File: ins_sort.fsx
 * Author: Franz Zbinden Garcia
 * Course: COTI 4039-KJ1
 * Date: 18/08/2024
 * Purpose: Implements recursive insertion sort for a list.
 * Features: Includes a helper function for inserting an element into a sorted list.
 *)

// Inserts an element into a sorted list in the correct position.
let rec insert elem lst =
    match lst with
    | [] -> [elem]
    | head :: _ when elem < head -> elem :: lst
    | head :: tail -> head :: insert elem tail

// Sorts a list using recursive insertion sort.
let rec insertionSort lst =
    match lst with
    | [] -> []
    | head :: tail -> insert head (insertionSort tail)

// Example usage of insertion and insertion sort functions.
let num = 4

// Demonstrates insertion into an empty list.
let emptyList = []
printfn "Empty list: %A" emptyList
let emptyList' = insert num emptyList
printfn "Inserting 4: %A" emptyList'

// Demonstrates insertion into a partially sorted list.
let nums = insert 4 [1; 2; 3; 5]
printfn "Inserting 4 into [1; 2; 3; 5]:"
printfn "   %A" nums

// Demonstrates sorting an unsorted list.
let unsortedNums = [5; 3; 8; 1; 2]
printfn "Unsorted list: %A" unsortedNums

let sortedNumbers = insertionSort unsortedNums
printfn "Sorted list: %A" sortedNumbers

// Bonus: Demonstrates inserting an element into a sorted list.
printfn ""
printfn "Bonus"
let sortedNumbers' = insert 7 sortedNumbers
printfn "Inserting 7 into the sorted list: %A" sortedNumbers'
