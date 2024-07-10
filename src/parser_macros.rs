macro_rules! impl_enum_equal {
    ($name:ident) => {
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                use std::mem::discriminant;
                discriminant(self) == discriminant(other)
            }
        }
    };
}
pub(crate) use impl_enum_equal;

macro_rules! assert_eq_or {
    ($left:expr, $right:expr) => {
        if $left != $right {
            let msg = format!(
                "{:?} != {:?}\nFile: {:?}\nLine: {:?}",
                $left,
                $right,
                file!(),
                line!()
            );
            return Err(Error::new(ErrorKind::InvalidData, msg));
        }
    };
}
pub(crate) use assert_eq_or;

macro_rules! assert_or {
    ($value:expr) => {
        if !($value) {
            let msg = format!(
                "ASSERTION FAILED: {:?}\nFile: {:?}\nLine: {:?}",
                $value,
                file!(),
                line!()
            );
            return Err(Error::new(ErrorKind::InvalidData, msg));
        }
    };
}
pub(crate) use assert_or;

macro_rules! throw_err {
    ($value:expr) => {
        let msg = format!("{:?}\nFile: {:?}\nLine: {:?}", $value, file!(), line!());
        return Err(Error::new(ErrorKind::InvalidData, msg));
    };
}
pub(crate) use throw_err;

macro_rules! throw_parse_err {
    ($start:expr, $end:expr, $explanation:expr) => {
        let msg = format!(
            "Error in Start: {:?}\nEnd: {:?}\nExplanation: {:?}\nFile: {:?}\nLine: {:?}",
            $start,
            $end,
            $explanation,
            file!(),
            line!()
        );
        return Err(Error::new(ErrorKind::InvalidData, msg));
    };
    ($text_data:expr, $explanation:expr) => {
        let msg = format!(
            "Error in Start: {:?}\nEnd: {:?}\nExplanation: {:?}\nFile: {:?}\nLine: {:?}",
            $text_data.start,
            $text_data.end,
            $explanation,
            file!(),
            line!()
        );
        return Err(Error::new(ErrorKind::InvalidData, msg));
    };
}
pub(crate) use throw_parse_err;
