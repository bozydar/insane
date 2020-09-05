use crate::error::Error;
use crate::file::File;
use crate::ident::Ident;
use crate::parse;

use crate::build_in_functions::build_in_functions;
use crate::ns_ident::NSIdent;
use crate::parse::Expr;
use std::env;
use std::fs;
use std::io::prelude::*;
use std::path;
use std::rc::Rc;

pub type Path = String;

#[derive(Debug, Clone)]
pub struct Context {
    pub look_path: Vec<String>,
    pub files: Vec<Rc<File>>,
    pub files_in_processing: Vec<String>,
}

impl Context {
    pub fn new(look_path: Vec<String>) -> Context {
        Context {
            look_path: Self::canonicalize_paths(look_path),
            files: vec![],
            files_in_processing: vec![],
        }
    }

    pub fn expr_by_ns_ident(&self, ns_ident: &NSIdent) -> Result<Rc<Expr>, Error> {
        if let Some(found_file) = self.find_in_files(&ns_ident.nspace) {
            let ident = Ident {
                label: ns_ident.label.clone(),
                position: ns_ident.position.clone(),
            };
            (*found_file).find_exposed(&ident)
        } else {
            Error::new(
                &format!("Can't find module `{}`", &ns_ident.nspace),
                &ns_ident.position,
            )
            .into()
        }
    }

    fn canonicalize_paths(look_path: Vec<String>) -> Vec<String> {
        let mut look_path_ = vec![];
        // Add current dir as a starting one on path
        let current_dir = env::current_dir().unwrap().to_str().unwrap().to_owned();
        look_path_.push(current_dir.clone());
        // dbg!(current_dir);
        for path in look_path.iter() {
            let path = fs::canonicalize(path).unwrap();
            look_path_.push(path.to_str().unwrap().to_string())
        }
        look_path_
    }

    pub fn load_file(&mut self, ident: &Ident) -> Result<Rc<File>, Error> {
        let module_name = &ident.label;
        if let Some(file) = self.find_in_files(module_name) {
            Ok(file)
        } else {
            let path = self
                .find_in_path(module_name)
                .map_err(|err| Error::new(&err, &ident.position))?;
            self.check_circularity(&path)
                .map_err(|err| Error::new(&err, &ident.position))?;
            let content =
                Self::read_content(&path).map_err(|err| Error::new(&err, &ident.position))?;
            self.files_in_processing.push(path.clone());
            let file = parse::parse_file(&content, &path, self)?;
            match &*file {
                parse::Expr::File(file) => {
                    let file = Rc::new(file.clone());
                    self.files.push(file.clone());
                    Ok(file)
                }
                _ => unreachable!(),
            }
        }
    }

    fn check_circularity(&self, to_check: &str) -> Result<(), String> {
        if self.files_in_processing.iter().any(|item| item == to_check) {
            Err(format!("Circular reference to `{}`", to_check))
        } else {
            Ok(())
        }
    }

    fn read_content(file_path: &str) -> Result<String, String> {
        match fs::File::open(file_path) {
            Ok(mut file) => {
                let mut content = String::new();
                match file.read_to_string(&mut content) {
                    Ok(_) => Ok(content),
                    Err(error) => Err(format!("{}", error)),
                }
            }
            Err(error) => Err(format!("{}", error)),
        }
    }

    fn find_in_path(&self, module_name: &str) -> Result<String, String> {
        for dir in self.look_path.iter() {
            let path = &path::Path::new(&*dir).join(format!("{}.sn", module_name));
            if path.exists() {
                return Ok(path.to_str().unwrap().to_string());
            }
        }
        let paths = self.look_path.join("\n");
        Err(format!(
            "Can't find module `{}` in paths `{}`",
            module_name, paths
        ))
    }

    pub fn find_in_files(&self, module_name: &str) -> Option<Rc<File>> {
        let a = self.files.iter().find(|file| &*file.name == module_name);
        match a {
            Some(file) => Some(file.clone()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::execute_file;
    use crate::parse::{parse_file, ToSource};

    // TODO Better tests in the proper directory

    #[test]
    fn load_0() {
        let context = &mut Context::new(vec![String::from("./src")]);
        let result = parse_file(
            r#"
        use (module_0)

        module_0.a"#,
            "ADHOC",
            context,
        )
        .unwrap();
        // TODO Why I can't find `module_0.a` in the context even though
        // it generates the source
        // dbg!(&context);

        assert_eq!(
            result.to_source(),
            r#"use (module_0)

module_0.a"#
        )
    }

    #[test]
    fn load_1() {
        let context = &mut Context::new(vec![String::from("./src")]);
        let scope = &mut build_in_functions();
        let result = execute_file(
            r#"
        use (module_1)

        [module_1.a; module_1.d]"#,
            "ADHOC",
            context,
            scope,
        )
        .unwrap()
        .to_source();

        // TODO Looks like it doesn't execute submodules
        assert_eq!(result, r#"[3.0; "module_1"]"#)
    }

    #[test]
    fn load_rec_err() {
        let context = &mut Context::new(vec![String::from("./src")]);
        let result = parse_file(
            r#"
        use (module_2)"#,
            "ADHOC",
            context,
        )
        .unwrap_err();

        assert_eq!(
            result.message,
            r#"Circular reference to `/Users/bozydarsobczak/Workspaces/insane/sane-core/src/module_2.sn`"#
        )
    }

    // #[test]
    // fn load_rec_0() {
    //     let context = &mut Context::new(r#"ADHOC"#, &["./src"]);
    //     let scope = &mut Scope::new();
    //     let result = parse_file(
    //         r#""#,
    //         context
    //     )
    //         .unwrap()
    //         .to_source();
    //
    //     // 1. Sprawdź czy faktycznie ładuje wszystkie moduły
    //     // 2. zastanów się czy w File nie powinien siedzieć `source_path` i `source`. Context jest
    //     // tylko na `look_path` i `files`
    //     assert_eq!(
    //         result.message,
    //         r#"Circular reference to `/Users/bozydarsobczak/Workspaces/insane/sane-core/src/module_2.sn`"#
    //     )
    // }
}
