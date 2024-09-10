use chumsky::{input::SliceInput, prelude::*};

use crate::util_new::Spanned;

pub trait ParserSpanned<'src, I, O, E>
where
    Self: Parser<'src, I, O, E> + Clone,
    I: Input<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
    fn spanned(self) -> impl Parser<'src, I, Spanned<O>, E> + Clone {
        self.map_with(|it, e| (it, e.span()))
    }
}

impl<'src, T, I, O, E> ParserSpanned<'src, I, O, E> for T
where
    T: Parser<'src, I, O, E> + Clone,
    I: Input<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
}

pub trait ParserSkipThenRetryUntil<'src, I, O, E>
where
    Self: Parser<'src, I, O, E> + Clone,
    I: SliceInput<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
    fn collect_while_fails(
        self,
        skip: impl Parser<'src, I, (), E> + Clone,
    ) -> impl Parser<'src, I, (I::Slice, Option<E::Error>), E> + Clone {
        custom(move |input| {
            let start = input.offset();
            let mut first_error = None;

            loop {
                // Are we at the end
                let before = input.save();
                if let Ok(()) = input.parse(end()) {
                    break Err(E::Error::expected_found([], None, input.span_from(start..)));
                }
                input.rewind(before);

                // Try to parse itself
                let before = input.save();
                let result = input.parse(self.clone());
                input.rewind(before);
                match result {
                    // Suceess, return everything before it
                    Ok(_) => break Ok((input.slice_since(start..), first_error)),
                    // Fail, record first error
                    Err(e) if first_error.is_none() => first_error = Some(e),
                    // Fail
                    Err(_) => {}
                }

                // Skip to next token
                let before = input.save();
                if let Err(e) = input.parse(skip.clone()) {
                    input.rewind(before);
                    break Err(e);
                }
            }
        })
    }
}

impl<'src, T, I, O, E> ParserSkipThenRetryUntil<'src, I, O, E> for T
where
    T: Parser<'src, I, O, E> + Clone,
    I: SliceInput<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
}

pub trait ParserCollectErrors<'src, I, O, E>
where
    Self: Parser<'src, I, O, E> + Clone,
    I: SliceInput<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I> + std::fmt::Debug,
{
    fn collect_errors(self) -> impl Parser<'src, I, Result<O, E::Error>, E> + Clone {
        custom(move |input| {
            let input = input.slice_from(input.offset()..);
            let result = self.parse(input);
            //let result = input.parse(self.clone());
            //Ok(result)
            todo!()
        })
    }
}

impl<'src, T, I, O, E> ParserCollectErrors<'src, I, O, E> for T
where
    T: Parser<'src, I, O, E> + Clone,
    I: SliceInput<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I> + std::fmt::Debug,
{
}
