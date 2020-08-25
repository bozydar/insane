extern crate sane_core;
use sane_core::execute::execute_sane;
use sane_core::parse::ToSource;

#[test]
fn test_reduce_0() {
    let result = execute_sane(
        r#"let reduce = fun fn =>
             let reduce_0 = fun acc list =>
                 if list |> count |> eq <| 0 then
                   acc
                 else
                   let h = list |> head in
                   let t = list |> tail in
                   let new_acc = fn(h acc) in
                     reduce_0(new_acc t)
             in reduce_0
           in
           [1; 2; 3] |> (reduce(add)) <| 0
        "#
    ).unwrap().to_source();
    assert_eq!(result, "6.0");
}

#[test]
fn test_reduce_1() {
    let result = execute_sane(
        r#"use (prelude)
           [1; 2; 3] |> (prelude.reduce(add)) <| 0
        "#
    ).unwrap().to_source();
    assert_eq!(result, "6.0");
}