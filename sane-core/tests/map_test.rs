extern crate sane_core;
use sane_core::execute::execute_sane;
use sane_core::parse::ToSource;

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
