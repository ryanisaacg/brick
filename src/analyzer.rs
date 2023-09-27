use crate::parser::ParsedSourceFile;

use self::resolve::resolve_module;

mod resolve;

pub fn typecheck(file: ParsedSourceFile) {
    let _module = resolve_module(file);
}
