extern crate sane_core;
use sane_core::execute::execute_sane;
use sane_core::parse::ToSource;
use std::env;


#[test]
fn test_reduce_0() {
    let result = execute_sane(
        r#"use (prelude)
           [1; 2; 3] |> prelude.reduce, add, 0
        "#,
    )
        .unwrap()
        .to_source();
    assert_eq!(result, "6.0");
}

#[test]
fn test_map_0() {
    let result = execute_sane(
        r#"use (prelude)
           [1; 2; 3] |> prelude.map, (fun x => x + 1)
        "#,
    )
        .unwrap()
        .to_source();
    assert_eq!(result, "[2.0; 3.0; 4.0]");
}

#[test]
fn test_nth_0() {
    let result = execute_sane(
        r#"use (prelude)
          prelude.nth, [1; 2; 3], 0
        "#,
    )
        .unwrap()
        .to_source();
    assert_eq!(result, "1.0");
}

// TODO Fix because causes stacks overflow
// #[test]
// fn test_nth_1() {
//     let result = execute_sane(
//         // Introduce gt lt to use in the function
//         r#"use (prelude)
//           prelude.nth, [1; 2; 3], -1
//         "#,
//     )
//         .unwrap()
//         .to_source();
//     assert_eq!(result, "Out of bound");
// }
