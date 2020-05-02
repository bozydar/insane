use crate::ident::Ident;
use core::cell::RefCell;
use crate::error::Error;
use crate::parse;
use crate::file::File;
use std::rc::Rc;
use std::collections::HashMap;
use std::fs;
use std::path;
use std::env;
use std::io::prelude::*;

pub type Path = String;

#[derive(Debug)]
pub struct Context {
    pub source: Rc<str>,
    file_loader: Rc<RefCell<FileLoader>>
}

impl Context {
    pub fn new(source: &str, look_path: &[&str]) -> Context {
        let file_loader = Rc::new(RefCell::new(FileLoader::new(look_path)));
        Context { 
            source: source.into(), 
            file_loader
        }
    }

    pub fn from_new_source(&self, source: &str) -> Context {
        Context { 
            source: source.into(), 
            file_loader: self.file_loader.clone()
        }
    }

    pub fn source_name(&self) -> String {
        let path = path::Path::new(&*self.source);
        path.file_stem()
            .unwrap().to_str()
            .unwrap().to_string()
    }

    pub fn load_file(&mut self, ident: &Ident) -> Result<Rc<File>, Error> {
        let mut file_loader = self.file_loader.borrow_mut();
        file_loader.load(ident, self)
    }
}

#[derive(Debug)]
struct FileLoader {
    pub look_path: Vec<String>,
    pub files: Vec<Rc<File>>
}

impl FileLoader {
    pub fn new(look_path: &[&str]) -> FileLoader {
        let mut look_path_ = vec![];
        // Add current dir as a starting one on path
        let current_dir = env::current_dir().unwrap().to_str().unwrap().to_owned();
        look_path_.push(current_dir);
        for path in look_path.iter() {
            let path = fs::canonicalize(path).unwrap();
            look_path_.push(path.to_str().unwrap().to_string())
        }
            
        let files = vec![];

        FileLoader {
            look_path: look_path_,
            files
        }
    }

    /// Let's assume for the very beginning that: 
    /// 0. All the modules will be described as absolute in the future
    /// 1. modules are files
    /// 2. Name of the module file's base name
    /// 3. Looking for a modules is:
    ///     1. Look for the module in the current directory
    ///     2. Look for the module in directories defined in look_path
    /// 4. Module is identified by name and it is the file found first
    pub fn load(&mut self, ident: &Ident, context: &Context) -> Result<Rc<File>, Error> {
        let module_name = &ident.label;
        if let Some(file) = self.find_in_files(module_name) {
            Ok(file)
        } else {
            let path = self.find_in_path(module_name)
                .map_err(|err| Error::new(&err, &ident.position))?;
            let content = FileLoader::read_content(&path)
                .map_err(|err| Error::new(&err, &ident.position))?;
            // TODO Can avoid such things when parsing to the direct type instead of Expr
            let context = &mut context.from_new_source(&path);
            let file = parse::parse_file(&content, context)?;
            match &*file {
                parse::Expr::File(file) => {
                    let file = Rc::new(file.clone());
                    self.files.push(file.clone());
                    Ok(file)
                }
                _ => unreachable!()
            }
        }
    }

    fn read_content(file_path: &str) -> Result<String, String> {
        match fs::File::open(file_path) {
            Ok(mut file) => {
                let mut content = String::new();
                match file.read_to_string(&mut content) {
                    Ok(_) => Ok(content),
                    Err(error) => Err(format!("{}", error))
                }
            },
            Err(error) => Err(format!("{}", error))
        }
    }


    fn find_in_path(&self, module_name: &str) -> Result<String, String> {
        for dir in self.look_path.iter() {
            let path = &path::Path::new(&*dir).join(format!("{}.sn", module_name));
            if path.exists() {
                return Ok(path.to_str().unwrap().to_string())
            }
        }
        let paths = self.look_path.join("\n");
        Err(format!("Can't find module `{}` in paths `{}`", module_name, paths))
    }

    fn find_in_files(&self, module_name: &str) -> Option<Rc<File>> {
        let a = self.files.iter()
            .find(|file| &*file.name == module_name);
        match a {
            Some(file) => Some(file.clone()),
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::execute_sane;
    use crate::parse::{parse_file, ToSource};
    use crate::file::File;

    // TODO Better tests in the proper firectory

    #[test]
    fn load_0() {
        let context = &mut Context::new(r#"ADHOC"#, &["./src"]);
        let result = parse_file(r#"
        use module_0

        1"#, context).unwrap();
        dbg!(&context);

        assert_eq!(result.to_source(), "3.0")
    }
}
