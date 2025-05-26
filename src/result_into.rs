pub trait ResultInto<T> {
    fn into(self) -> T;
}

impl<T1, T2, E> ResultInto<Result<T2, E>> for Result<T1, E>
where
    T1: Into<T2>,
{
    fn into(self) -> Result<T2, E> {
        self.map(|t| t.into())
    }
}

impl<T, E1, E2> ResultInto<Result<T, E2>> for Result<T, E1> {
    fn into(self) -> Result<T, E2> {}
}
