use crate::parse::Position;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Error {
    pub message: String,
    pub backtrace: Vec<Position>,
}

impl Error {
    pub fn new(message: &str, position: &Position) -> Self {
        Self {
            message: message.to_string(),
            backtrace: vec![position.clone()],
        }
    }

    pub fn push_backtrace_item(&mut self, position: &Position) -> &mut Self {
        self.backtrace.push(position.clone());
        self
    }
}
