extern crate sane_core;
use sane_core::execute::execute_sane;
use sane_core::parse::ToSource;

#[test]
fn test_map_0() {
    let result = execute_sane(
        r#"let map = fun fn =>
                let map_ = fun list =>
                if eq(count(list) 0) then
                    []
                else
                    let h = fn(head(list)) in
                    let t = tail(list) in
                    concat([h] map_(t))
                in map_
            in
                (map(inc))([1; 2; 3])
        "#
    ).unwrap().to_source();
    assert_eq!(result, "[2.0; 3.0; 4.0]");
}
