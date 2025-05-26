#[macro_export]
macro_rules! group {
    (
        $(#[$attr:meta])*
        $visibility:vis enum $group:ident {
            $($variant:ident($variant_type:ty)),*
        }
    ) => {
        $(#[$attr])*

        $visibility enum $group {
            $(
                $variant($variant_type),
            )*
        }

        $(
            impl From<$variant_type> for $group {
                fn from(value: $variant_type) -> Self {
                    $group::$variant(value)
                }
            }
        )*
    }
}
