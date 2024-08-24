use std::{fmt, ops::Deref};

use chumsky::span::SimpleSpan;

pub struct Spanned<T> {
    pub value: T,
    pub span: SimpleSpan,
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.span == other.span
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, S> From<(T, S)> for Spanned<T>
where
    S: Into<SimpleSpan>,
{
    fn from((value, span): (T, S)) -> Self {
        Self {
            value,
            span: span.into(),
        }
    }
}
