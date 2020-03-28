extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

mod error;
mod parse;
mod let_in;
mod ident;
mod const_expr;
mod bind;
mod fun;
mod list;
mod if_then_else;
mod build_in;
mod execute;
mod build_in_functions;


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_number() {
        let result = &*parse_sane("-23.1").unwrap();
        assert_eq!(result, &Expr::Const(Const { value: ConstType::Numeric(-23.1), position: Position { start: 0, end: 5 } }));
    }

    #[test]
    fn parse_string() {
        let result = &*parse_sane("\"test\"").unwrap();
        assert_eq!(result, &Expr::Const(Const { value: ConstType::String("test".into()), position: Position { start: 1, end: 5 } }));
    }

    #[test]
    fn parse_let_in_0() {
        let result = parse_sane("let a = 1 in a").unwrap().to_source();
        assert_eq!(result, "let a = 1.0 in a");
    }

    #[test]
    fn parse_let_in_1() {
        let result = parse_sane("let a = 1 and let b = 2 in [a; b]").unwrap().to_source();
        assert_eq!(result, "let a = 1.0 and let b = 2.0 in [a; b]");
    }

    #[test]
    fn execute_let_in_1() {
        let result = execute_sane("let a = 1 and let b = 2 in [a; b]").unwrap().to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }

    #[test]
    fn parse_bind_1() {
        let result = parse_sane("app 2 to app 1 to f").unwrap().to_source();
        assert_eq!(result, "app 2.0 to app 1.0 to f");
    }

    #[test]
    fn parse_fun() {
        let result = &*parse_sane("fun a => a").unwrap().to_source();
        assert_eq!(result, "fun a => a");
    }

    #[test]
    fn test_execute_let_0() {
        let result = &*execute_sane("let a = 1 in a").unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_execute_let_1() {
        let result = &*execute_sane("let a = let b = 1 in b in a").unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_execute_bind_0() {
        let result = &*execute_sane("app 2 to (fun a => a)").unwrap().to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_bind_1() {
        let result = &*execute_sane("let f = fun a => a in app  2 to f").unwrap().to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_bind_2() {
        let result = &*execute_sane("let f = fun a => fun b => a in app 1 to app 2 to f").unwrap().to_source();
        assert_eq!(result, "2");
    }

    #[test]
    fn test_execute_bind_3() {
        let result = &*execute_sane("let f = fun a => fun b => b in app 1 to app 2 to f").unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_execute_array_0() {
        let result = execute_sane("[1; \"two\"; fun a => a]").unwrap().to_source();
        assert_eq!(result, r#"[1.0; "two"; fun a => a]"#);
    }

    #[test]
    fn test_execute_array_1() {
        let result = execute_sane("[1; [2; [3]]]").unwrap().to_source();
        assert_eq!(result, "[1.0; [2.0; [3.0]]]");
    }

    #[test]
    fn test_execute_array_2() {
        let result = execute_sane(r#"[1; "two"; fun a => a]"#).unwrap().to_source();
        assert_eq!(result, r#"[1.0; "two"; fun a => a]"#);
    }

    #[test]
    fn test_execute_head_1() {
        let result = &*execute_sane("app [1] to  head").unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_execute_head_2() {
        let result = &*execute_sane("app [1] to  head").unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_execute_tail_0() {
        let result = &*execute_sane("app [] to tail").unwrap().to_source();
        assert_eq!(result, "[]");
    }

    // #[test]
    // fn test_execute_tail_1() {
    //     let result = &*execute_sane("app [1] to tail").unwrap();
    //     assert_eq!(result, &Expr::List(
    //         List {
    //             items: vec![]
    //         }
    //     ));
    // }

    // #[test]
    // fn test_execute_tail_2() {
    //     let result = &*execute_sane("app [1;2] to tail").unwrap();
    //     assert_eq!(result, &Expr::List(
    //         List {
    //             items: vec![
    //                 Rc::new(Expr::Const(Const::Numeric(2.0)))
    //             ]
    //         }
    //     ));
    // }

    // #[test]
    // fn test_execute_eq_0() {
    //     let result = &*execute_sane("app 1, 1 to eq").unwrap();
    //     assert_eq!(result, &Expr::Const(Const::Bool(true)));
    // }

    // #[test]
    // fn test_execute_eq_1() {
    //     let result = &*execute_sane("app fun a => b, fun a => b to eq").unwrap();
    //     assert_eq!(result, &Expr::Const(Const::Bool(true)));
    // }

    // #[test]
    // fn test_execute_eq_2() {
    //     let result = &*execute_sane("app fun a => b, fun a => c to eq").unwrap();
    //     assert_eq!(result, &Expr::Const(Const::Bool(false)));
    // }

    // #[test]
    // fn test_execute_eq_3() {
    //     let result = &*execute_sane("app [], [] to eq").unwrap();
    //     assert_eq!(result, &Expr::Const(Const::Bool(true)));
    // }

    // #[test]
    // fn test_execute_eq_4() {
    //     let result = &*execute_sane(
    //         r#"let a = fun b => b in
    //            let c = fun d => d in
    //              app app 1 to c, app 1 to a to eq"#).unwrap(); // eq(c(1), a(1))
    //     assert_eq!(result, &Expr::Const(Const::Bool(true)));
    // }

    // #[test]
    // fn test_execute_eq_5() {
    //     let result = &*execute_sane(
    //         r#"let eqa = fun left =>
    //               let eqa_ = fun right =>
    //                 app left, right to eq
    //               in eqa_
    //            in app 1 to app 1 to eqa"#).unwrap();
    //     assert_eq!(result, &Expr::Const(Const::Bool(true)));
    // }

    // #[test]
    // fn test_execute_add_0() {
    //     let result = &*execute_sane(
    //         r#"app 1, 2 to add"#).unwrap();
    //     assert_eq!(result, &Expr::Const(Const::Numeric(3.0)));
    // }

    // #[test]
    // fn test_execute_curry_0() {
    //     let result = &*execute_sane(
    //         r#"let c = fun a =>
    //              fun b =>
    //                app a, b to eq
    //            in
    //              let curr = app 1 to c
    //              in
    //                app 2 to curr"#).unwrap();
    //     assert_eq!(result, &Expr::Const(Const::Bool(false)));
    // }

    #[test]
    fn test_map_0() {
        let result = execute_sane(
            r#"let map = fun fn =>
                 let map_ = fun list =>
                   if app (app list to count), 0 to eq then
                     []
                   else
                     let h = app (app list to head) to fn in
                     let t = app list to tail in
                     app [h], (app t to map_) to concat
                 in map_
               in
                 app [1; 2; 3] to app (fun a => app a to inc) to map
            "# // map((a) => a + 1)(new List<int>(1, 2, 3))
        ).unwrap().to_source();
        assert_eq!(result, "[2.0; 3.0; 4.0]");
    }

    // #[test]
    // fn test_reduce_0() {
    //     let result = execute_sane(
    //         r#"let reduce = fun fn =>
    //              let reduce_0 = fun acc list =>
    //                  if app (app (app list to print) to count), 0 to eq then
    //                    acc
    //                  else
    //                    let h = app list to head in
    //                    let t = app list to tail in
    //                    let new_acc = app h, acc to fn in
    //                      app new_acc, t to reduce_0
    //              in reduce_0
    //            in
    //              let sum = add
    //              in
    //                app 0, [1; 2; 3] to app sum to reduce
    //         "#
    //     ).unwrap().to_source();
    //     assert_eq!(result, "6.0");
    // }

    // #[test]
    // fn test_recursive_0() {
    //     let result = execute_sane(
    //         r#"let add_till_10 = fun a =>
    //              if app a, 10.0 to eq then a else app (app a to inc) to add_till_10
    //            in app 0 to add_till_10"#).unwrap().to_source();
    //     assert_eq!(result, "10.0");
    // }

    // #[test]
    // fn test_recursive_1() {
    //     let result = execute_sane(
    //         r#"let flip = fun a =>
    //              if (app a, 10.0 to eq) then a else (app (app a to inc) to flop)
    //            and let flop = fun b =>
    //              app b to flip
    //            in app 0 to flip"#).unwrap().to_source();
    //     assert_eq!(result, "10.0");
    // }

    // // #[test]
    // // fn test_recursive_2() {
    // //     let result = execute_sane(
    // //         r#"let flip = fun a =>
    // //              if (app a, 10.0 to eq) then a else (app (app a to inc) to flop)
    // //            in let flop = fun b =>
    // //              app b to flip
    // //            in app 0 to flip"#);
    // //     assert_eq!(result, Err("Ident `flop` not found".to_string()));
    // // }

    // #[test]
    // fn test_execute_inc_0() {
    //     let result = execute_sane(
    //         r#"let a = 1 in
    //              app a to inc"#).unwrap().to_source();
    //     assert_eq!(result, "2.0");
    // }

    // #[test]
    // fn test_execute_if_0() {
    //     let result = execute_sane(
    //         r#"if true then 1 else 2"#).unwrap().to_source();
    //     assert_eq!(result, "1.0");
    // }

    // #[test]
    // fn test_execute_if_1() {
    //     let result = execute_sane(
    //         r#"let plus_one = fun a =>
    //              app a to inc
    //            in
    //            let b = app 1 to plus_one in
    //            if app b, 2 to eq then 1 else 2"#).unwrap().to_source();
    //     assert_eq!(result, "1.0");
    // }

    // #[test]
    // fn test_count_0() {
    //     let result = execute_sane(
    //         r#"app [] to count"#).unwrap().to_source();
    //     assert_eq!(result, "0.0");
    // }

    // #[test]
    // fn test_count_1() {
    //     let result = execute_sane(
    //         r#"app [1] to count"#).unwrap().to_source();
    //     assert_eq!(result, "1.0");
    // }

    // #[test]
    // fn test_concat_0() {
    //     let result = execute_sane(
    //         r#"app [1], [2] to concat"#).unwrap().to_source();
    //     assert_eq!(result, "[1.0; 2.0]");
    // }

    // #[test]
    // fn test_auto_curr_0() {
    //     let result = parse_sane(
    //         r#"let f = fun a b c => a
    //            in app 1 to f"#).unwrap().to_source();
    //     assert_eq!(result, "let f = fun a b c => a in app 1.0 to f");
    // }

    // #[test]
    // fn test_auto_curr_01() {
    //     let result = execute_sane(
    //         r#"let f = fun a b c => a
    //            in app 1 to f"#).unwrap().to_source();
    //     assert_eq!(result, "fun $param_0 $param_1 => app 1.0, $param_0, $param_1 to fun a b c => a");
    // }

    // #[test]
    // fn test_auto_curr_02() {
    //     let result = execute_sane(
    //         r#"let f = fun a b c => [a; b; c]
    //            in let g = app 1 to f
    //            in app 2, 3 to g"#).unwrap().to_source();
    //     assert_eq!(result, "[1.0; 2.0; 3.0]");
    // }

    // #[test]
    // fn test_auto_curr_1() {
    //     let result = execute_sane(
    //         r#"let f = fun a b => app a, b to add in
    //            let my_inc = app 1 to f in
    //            app 2 to my_inc"#).unwrap().to_source();
    //     assert_eq!(result, "3.0");
    // }

    // #[test]
    // fn test_auto_curr_2() {
    //     let result = execute_sane(
    //         r#"let f = add in
    //            let my_inc = app 1 to f in
    //            app 2 to my_inc"#).unwrap().to_source();
    //     assert_eq!(result, "3.0");
    // }

    // #[test]
    // fn test_auto_curr_3() {
    //     let result = execute_sane(
    //         r#"let my_inc = app 1 to add in
    //            app 2 to my_inc"#).unwrap().to_source();
    //     assert_eq!(result, "3.0");
    // }
}
