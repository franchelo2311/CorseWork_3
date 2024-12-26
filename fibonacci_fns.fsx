(* 
 * File: fibonacci_fns.fsx
 * Author: Franz Zbinden Garcia
 * Course: COTI 4039-KJ1
 * Date: 18/08/2024
 * Purpose: Computes and displays the Fibonacci sequence using various techniques.
 * such as recursion, pattern matching, tail recursion, and higher-order functions.
 * Includes sequence generation for additional visualization.
 *)

// Computes Fibonacci numbers using standard recursion.
let rec fibo num = // int -> int
   if num <= 1 then num 
   else fibo (num - 1) + fibo (num - 2)

// Computes Fibonacci numbers using pattern matching.
let rec fiboMatch num = // int -> int
   match num with
   | 0 -> num
   | 1 -> num
   | _ -> fiboMatch (num - 2) + fiboMatch (num - 1)

// Computes Fibonacci numbers using tail recursion.
let fiboIter num = 
   let rec loop a b count = 
      match count with 
      | 0 -> a
      | _ -> loop b (a + b) (count - 1)
   loop 0 1 num

// Generalized Fibonacci computation using a higher-order.
let rec fibonacciGen f a b n =
    match n with
    | 0 -> a
    | _ -> fibonacciGen f b (f a b) (n - 1)

// Computes Fibonacci numbers using a higher-order.
let fiboHigher num = 
   match num with
   | num when num < 0 -> failwith "The given number must be positive"
   | _ -> fibonacciGen (fun a b -> a + b) 0 1 num

// Generates a sequence of Fibonacci numbers.
let fiboSeq = seq {0..200} |> Seq.map (fun num -> fibo num)

// Example usage: Compute the 7th term using various methods.
let num = 7

printfn "Computing the 7th term of the Fibonacci sequence:"
printfn "      Using if expression: %d" (fibo num)
printfn "      Using pattern matching: %d" (fiboMatch num)
printfn "      Using tail-recursive helper function: %d" (fiboIter num)
printfn "      Using higher-order function: %d" (fiboHigher num)
printfn "The Fibonacci sequence is: %A" fiboSeq
