macro_rules! split_tokens {
    ($vec:ident, $container_manager: ident, $separator: expr, $ptr_buff: ident, $target: ident) => {
        for (i, tk) in $vec.iter().enumerate() {
            if i == 0 || i == $vec.len() - 1 {
                continue;
            }
            $container_manager.check(tk);

            if !$container_manager.is_free() {
                continue;
            }

            if *tk == $separator || i == $vec.len() - 2 {
                let range = if i == $vec.len() - 2 {
                    ($ptr_buff)..(i + 1)
                } else {
                    ($ptr_buff)..i
                };

                $target.push(range);

                $ptr_buff = i + 1;
                continue;
            }
        }
    };
}
pub(crate) use split_tokens;

macro_rules! apply_range {
    ($container:ident, $ranges:ident, $target: ident) => {
        for range in $ranges {
            $target.push($container[range].to_vec());
        }
    };
}
pub(crate) use apply_range;

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
            let msg = format!("{:?} != {:?}", $left, $right);
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
    ($value:literal) => {
        return Err(Error::new(ErrorKind::InvalidData, $value));
    };
}
pub(crate) use throw_err;
