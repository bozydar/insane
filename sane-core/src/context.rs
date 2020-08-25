use crate::error::Error;
use crate::file::File;
use crate::ident::Ident;
use crate::parse;
use core::cell::RefCell;
use std::env;
use std::fs;
use std::io::prelude::*;
use std::path;
use std::rc::Rc;
use crate::parse::Expr;
use crate::ns_ident::NSIdent;
use crate::build_in_functions::build_in_functions;
use std::fs::canonicalize;

pub type Path = String;

#[derive(Debug, Clone)]
pub struct Context {
    pub source: Rc<str>,
    pub look_path: Vec<String>,
    pub files: Vec<Rc<File>>,
    pub files_in_processing: Vec<String>
}


impl Context {
    pub fn new(source: &str, look_path: Vec<String>) -> Context {
        Context {
            source: source.into(),
            look_path: Self::canonicalize_paths(look_path),
            files: vec![],
            files_in_processing: vec![]
        }
    }

    pub fn from_new_source(&self, source: &str) -> Context {
        Context {
            source: source.into(),
            look_path: self.look_path.clone(),
            files: self.files.clone(),
            files_in_processing: self.files_in_processing.clone()
        }
    }

    pub fn source_name(&self) -> String {
        let path = path::Path::new(&*self.source);
        path.file_stem().unwrap().to_str().unwrap().to_string()
    }

    pub fn load_file(&mut self, ident: &Ident) -> Result<Rc<File>, Error> {
        self.load(ident)
    }

    pub fn expr_by_ns_ident(&self, ns_ident: &NSIdent) -> Result<Rc<Expr>, Error> {
        if let Some(found_file) = self
            .find_in_files(&ns_ident.nspace) {
            let scope = &mut build_in_functions();
            // TODO The problem is that "self" context is context of file which calls the module
            // but we don't have the contexts of the called module/file.
            // Looks like File should keep `source: Rc<str>` as well to create Context
            // Not sure if Context should keep sources...
            let context = self.from_new_source(&*found_file.name);
            let ident = Ident {
                label: ns_ident.label.clone(),
                position: ns_ident.position.clone()
            };
            (*found_file).execute_exposed(scope, &context, &ident)
        } else {
            Error::new(
                &format!("Can't find module `{}`", &ns_ident.nspace),
                &ns_ident.position
            ).into()
        }
    }

    fn canonicalize_paths(look_path: Vec<String>) -> Vec<String> {
        let mut look_path_ = vec![];
        // Add current dir as a starting one on path
        let current_dir = env::current_dir().unwrap().to_str().unwrap().to_owned();
        look_path_.push(current_dir.clone());
        dbg!(current_dir);
        for path in look_path.iter() {
            let path = fs::canonicalize(path).unwrap();
            look_path_.push(path.to_str().unwrap().to_string())
        }
        look_path_
    }

    /// Let's assume for the very beginning that:
    /// 0. All the modules will be described as absolute in the future
    /// 1. modules are files
    /// 2. Name of the module file's base name
    /// 3. Looking for a modules is:
    ///     1. Look for the module in the current directory
    ///     2. Look for the module in directories defined in look_path
    /// 4. Module is identified by name and it is the file found first
    pub fn load(&mut self, ident: &Ident) -> Result<Rc<File>, Error> {
        let module_name = &ident.label;
        if let Some(file) = self.find_in_files(module_name) {
            Ok(file)
        } else {
            let path = self
                .find_in_path(module_name)
                .map_err(|err| Error::new(&err, &ident.position))?;
            self.check_circularity(&path)
                .map_err(|err| Error::new(&err, &ident.position))?;
            let content = Self::read_content(&path).map_err(|err| Error::new(&err, &ident.position))?;
            self.files_in_processing.push(path.clone());
            let context = &mut self.from_new_source(&path);
            let file = parse::parse_file(&content, context)?;
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
    use crate::parse::{parse_file, ToSource};
    use crate::execute::{execute_file, Scope};

    // TODO Better tests in the proper directory

    #[test]
    fn load_0() {
        let context = &mut Context::new(r#"ADHOC"#, vec![String::from("./src")]);
        let result = parse_file(
            r#"
        use (module_0)

        module_0.a"#,
            context,
        )
        .unwrap();
        // TODO Why I can't find `module_0.a` in the context even though
        // it generates the source
        dbg!(&context);

        assert_eq!(
            result.to_source(),
            r#"use (module_0)

module_0.a"#
        )
    }

    #[test]
    fn load_1() {
        let context = &mut Context::new(r#"ADHOC"#, vec![String::from("./sane-core/src")]);
        let scope = &mut Scope::new();
        let result = execute_file(
            r#"
        use (module_1)

        [module_1.a; module_1.d]"#,
            context, scope
        )
            .unwrap()
            .to_source();

        // TODO Looks like it doesn't execute submodules
        assert_eq!(
            result,
            r#"[1.0]"#
        )
    }

    #[test]
    fn load_rec_err() {
        let context = &mut Context::new(r#"ADHOC"#, vec![String::from("./src")]);
        let scope = &mut Scope::new();
        let result = parse_file(
            r#"
        use (module_2)"#,
            context
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
