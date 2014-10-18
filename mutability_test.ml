open Assertions
open Mutability

(* count_up *)

let c = count_up_from 3 2
let d = count_up_from 0 0
let e = count_up_from 4 (-1)
let f = count_up_from (-2) (-1)

TEST_UNIT "count_up_from_test1" = assert_true ( c() = 3)
TEST_UNIT "count_up_from_test2" = assert_true ( c() = 5)
TEST_UNIT "count_up_from_test3" = assert_true ( c() = 7)

TEST_UNIT "count_up_from_test4" = assert_true ( d() = 0)
TEST_UNIT "count_up_from_test5" = assert_true ( d() = 0)
TEST_UNIT "count_up_from_test6" = assert_true ( d() = 0)

TEST_UNIT "count_up_from_test7" = assert_true ( e() = 4)
TEST_UNIT "count_up_from_test8" = assert_true ( e() = 3)
TEST_UNIT "count_up_from_test9" = assert_true ( e() = 2)

TEST_UNIT "count_up_from_test10" = assert_true ( f() = (-2))
TEST_UNIT "count_up_from_test11" = assert_true ( f() = (-3))
TEST_UNIT "count_up_from_test12" = assert_true ( f() = (-4))

(* tabulate *)

TEST_UNIT "tabulate_test1" = assert_true ( tabulate (fun x -> x*x) 5 = [|0; 1; 4; 9; 16|])
TEST_UNIT "tabulate_test2" = assert_true ( tabulate (fun x -> x*x) 0 = [||])
TEST_UNIT "tabulate_test3" = assert_true ( tabulate (fun x -> x*x) 1 = [|0|])

(* fold_left_imp *)

let f (a:int) (x:int) = x+a
let g (a:int) (x:int) = x*a
let m (a:string) (b:string) = a^b

TEST_UNIT "fold_left_imp_test1" = assert_true ( fold_left_imp f 0 [1;2;3;4] = 10)
TEST_UNIT "fold_left_imp_test2" = assert_true ( fold_left_imp g 1 [1;2;3;4] = 24)
TEST_UNIT "fold_left_imp_test3" = assert_true ( fold_left_imp m "" ["hi"; "anu"] = "hianu")
TEST_UNIT "fold_left_imp_test4" = assert_true ( fold_left_imp f 0 [] = 0)

(* zardoz *)

TEST_UNIT "zardoz_test1" = assert_false ( List.map zardoz (List.rev lst)  =  List.rev (List.map zardoz lst) )

let () = Pa_ounit_lib.Runtime.summarize ()