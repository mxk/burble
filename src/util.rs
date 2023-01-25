pub(crate) use condvar::*;

mod condvar;

/// Returns a string representation of the specified type.
macro_rules! name_of {
    ($t:ty) => {{
        // TODO: Switch to `std::any::type_name` when stabilized
        type _T = $t; // Allows $t to be recognized as a type for refactoring
        stringify!($t)
    }};
}
pub(crate) use name_of;
