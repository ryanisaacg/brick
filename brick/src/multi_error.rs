use std::fmt::Display;

pub trait MultiError: Sized {
    fn from_error_list(list: Vec<Self>) -> Self;
    fn as_error_list(&mut self) -> Option<&mut Vec<Self>>;
}

pub fn merge_results<E: MultiError>(current: &mut Result<(), E>, new: Result<(), E>) {
    match (current.as_mut(), new) {
        (Ok(_), Ok(_)) | (Err(_), Ok(_)) => {}
        (Ok(_), new @ Err(_)) => {
            *current = new;
        }
        (Err(old), Err(mut new)) => match (old.as_error_list(), new.as_error_list()) {
            (Some(old), Some(new)) => {
                old.append(new);
            }
            (Some(old), None) => {
                old.push(new);
            }
            (_, Some(list)) => {
                let mut temp = Ok(());
                std::mem::swap(&mut temp, current);
                list.push(temp.unwrap_err());
                *current = Err(E::from_error_list(std::mem::take(list)));
            }
            (_, None) => {
                let mut temp = Ok(());
                std::mem::swap(&mut temp, current);
                *current = Err(E::from_error_list(vec![temp.unwrap_err(), new]));
            }
        },
    }
}

pub fn print_multi_errors<E: Display>(errors: &[E]) -> String {
    let mut string = String::new();
    for error in errors.iter() {
        string.push_str(&format!("{error}\n"));
    }
    string
}
