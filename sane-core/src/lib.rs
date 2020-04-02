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
    use crate::execute::execute_sane;
    use crate::parse::ToSource;

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

    #[test]
    fn test_reduce_0() {
        let result = execute_sane(
            r#"let reduce = fun fn =>
                 let reduce_0 = fun acc list =>
                     if app (app (app list to print) to count), 0 to eq then
                       acc
                     else
                       let h = app list to head in
                       let t = app list to tail in
                       let new_acc = app h, acc to fn in
                         app new_acc, t to reduce_0
                 in reduce_0
               in
                 let sum = add
                 in
                   app 0, [1; 2; 3] to app sum to reduce
            "#
        ).unwrap().to_source();
        assert_eq!(result, "6.0");
    }

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

    // #[test]
    // fn test_recursive_2() {
    //     let result = execute_sane(
    //         r#"let flip = fun a =>
    //              if (app a, 10.0 to eq) then a else (app (app a to inc) to flop)
    //            in let flop = fun b =>
    //              app b to flip
    //            in app 0 to flip"#);
    //     assert_eq!(result, Err("Ident `flop` not found".to_string()));
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
