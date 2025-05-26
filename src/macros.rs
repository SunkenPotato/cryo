#[macro_export]
macro_rules! error_group {
    (
        $(#[$attr:meta])*
        $visibility:vis enum $identifier:ident {
            $($variant_id:ident),*
        }
    ) => {
        $(
            #[$attr]
        )*
        $visibility enum $identifier {
            $(
                $variant_id($variant_id),
            )*
        }

        $(
            impl From<$variant_id> for $identifier {
                fn from(value: $variant_id) -> Self {
                    Self::$variant_id(value)
                }
            }
        )*
    };
}
