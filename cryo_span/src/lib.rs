//! Spans for the `cryo` language.
//!
//! A span declares a range inside a file.

use std::ops::{Add, AddAssign, Deref, DerefMut};

/// A span.
///
/// A span contains the start and the stop of the section it refers to. It acts similar to a range.
///
/// ## Examples
/// ```rust
/// use cryo_span::Span;
///
/// Span::new(0, 20);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    /// The lower bound of this [`Span`].
    pub start: u32,
    /// The upper bound of this [`Span`].
    pub stop: u32,
}

impl Span {
    /// A zero span. It has the file `0`, the start `0` and the stop `0`.
    pub const ZERO: Self = Span::new(0, 0);

    /// Create a new [`Span`] from a start and a stop.
    ///
    /// If `start > stop`, this function will panic.
    #[inline]
    #[track_caller]
    pub const fn new(start: u32, stop: u32) -> Self {
        debug_assert!(start <= stop);
        Self { start, stop }
    }

    /// Offset the `start` and `stop` of the [`Span`] by `offset`.
    pub const fn offset(mut self, offset: u32) -> Self {
        self.start += offset;
        self.stop += offset;

        self
    }
}

impl Add for Span {
    type Output = Self;
    fn add(mut self, rhs: Self) -> Self::Output {
        if rhs.stop >= self.stop {
            self.stop = rhs.stop;
        }
        self
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

/// A spanned generic.
///
/// This struct is simply the generic and a [`Span`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    /// The given type.
    pub t: T,
    /// The span.
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new [`Spanned`] from `T` and a [`Span`].
    #[inline]
    pub const fn new(t: T, span: Span) -> Self {
        Self { t, span }
    }

    /// Create a new [`Spanned`] from a value and the span [`Span::ZERO`].
    #[inline]
    pub const fn zero(t: T) -> Self {
        Self::new(t, Span::ZERO)
    }

    /// Map a `Spanned<T>` to a `Spanned<U>` via a given function.
    pub fn map<F, U>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            t: f(self.t),
            span: self.span,
        }
    }

    /// Extend the inner span by the given span.
    ///
    /// This method is equivalent to adding `span` to the inner ]`Span`].
    pub fn extend(mut self, span: Span) -> Self {
        self.span += span;
        self
    }

    /// Offset the inner [`Span`] by `offset`.
    ///
    /// This method is equivalent to calling [`Span::offset`] on the inner span.
    pub const fn offset(mut self, offset: u32) -> Self {
        self.span = self.span.offset(offset);
        self
    }

    /// Reinterpret this `Spanned<T>` as a `(T, Span)`.
    #[inline]
    pub fn tuple(self) -> (T, Span) {
        (self.t, self.span)
    }

    /// Equivalent to `self.map(From::from)`.
    #[inline]
    pub fn cvt<U>(self) -> Spanned<U>
    where
        U: From<T>,
    {
        self.map(From::from)
    }

    /// Try to convert a `Spanned<T>` into a `Spanned<U>` via `TryInto<U>`.
    pub fn try_cvt<U>(self) -> Result<Spanned<U>, <T as TryInto<U>>::Error>
    where
        T: TryInto<U>,
    {
        Ok(Spanned {
            t: self.t.try_into()?,
            span: self.span,
        })
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.t
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.t
    }
}
