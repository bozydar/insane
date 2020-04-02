extern crate sane_core;
use sane_core::execute::execute_sane;
use sane_core::parse::ToSource;

#[test]
fn test_reduce_0() {
    let result = execute_sane(
        r#"let reduce = fun fn =>
             let reduce_0 = fun acc list =>
                 if app (app list to count), 0 to eq then
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
