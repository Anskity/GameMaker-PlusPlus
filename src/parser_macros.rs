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
