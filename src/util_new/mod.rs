use chumsky::span::SimpleSpan;

pub type Spanned<T> = (T, SimpleSpan);
