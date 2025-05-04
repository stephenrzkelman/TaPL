open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isnumericval t = match t with
    TmZero(_) -> true
    | TmSucc(_, t_val) -> isnumericval t_val
    | _ -> false

let rec isval t = match t with
    TmTrue(_) -> true
    | TmFalse(_) -> true
    | t_val when isnumericval t_val -> true
    | _ -> false


(* Exercise 4.2.1: Re-implement eval without needing a handler in the recursive loop *)

let rec singlestep t = match t with
    TmIf(_, TmTrue(_), t_then, t_else) -> t_then
    | TmIf(_, TmFalse(_), t_then, t_else) -> t_else
    | TmIf(blah, t_cond, t_then, t_else) -> 
        let t1_cond = singlestep t_cond in
        TmIf(blah, t1_cond, t_then, t_else)
    | TmSucc(blah, t_val) ->
        let t1_val = singlestep t_val in
        TmSucc(blah, t1_val)
    | TmPred(_, TmZero(_)) -> TmZero(dummyinfo)
    | TmPred(_, TmSucc(_, t_val)) when isnumericval t_val -> t_val
    | TmPred(blah, t_val) -> 
        let t1_val = singlestep t_val in
        TmPred(blah, t1_val)
    | TmIsZero(_, TmZero(_)) -> TmTrue(dummyinfo)
    | TmIsZero(_, TmSucc(_,t_val)) when isnumericval t_val -> TmFalse(dummyinfo)
    | TmIsZero(blah, t_val) -> 
        let t1_val = singlestep t_val in
        TmIsZero(blah, t1_val)
    | _ -> t


let rec singlestep_eval t =
    let t1 = singlestep t in 
    if t1 = t 
    then t
    else singlestep_eval t1


(* Exercise 4.2.2: Implement eval in the big-step style *)

let rec bigstep t = match t with
    TmIf(_, t_cond, t_then, t_else) -> (
        match bigstep t_cond with
            TmTrue(_) -> bigstep t_then
            | TmFalse(_) -> bigstep t_else
            | _ -> t
    )
    | TmSucc(blah, t_val)  -> (
        let t1_val = bigstep t_val in
        if isnumericval t1_val
        then TmSucc(blah, t1_val)
        else t
    )
    | TmPred(blah, t_val) -> (
        match bigstep t_val with
            TmZero(_) -> TmZero(dummyinfo)
            | TmSucc(_, t_val) when isnumericval t_val -> 
                bigstep t_val
            | _ -> t
    )
    | TmIsZero(_, t_val) -> (
        match bigstep t_val with
            TmZero(_) -> TmTrue(dummyinfo)
            | TmSucc(_, _) -> TmFalse(dummyinfo)
            | _ -> t
    )
    | _ -> t

let rec bigstep_eval t =
    let t1 = bigstep t in 
    if t1 = t 
    then t
    else bigstep_eval t1