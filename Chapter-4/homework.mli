(* module Homework

   My own typechecking and evaluation functions
*)

open Syntax
open Support.Error

val singlestep_eval : term -> term 
val bigstep_eval : term -> term