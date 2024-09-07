use crate::util_new::Spanned;

use chumsky::{
    error::{Error, Rich},
    prelude::*,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenError {
    Unrecognized,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'src> {
    Err(TokenError),

    Label(&'src str),
}

pub type Token<'src> = Spanned<TokenKind<'src>>;

macro_rules! parser_fn {
    (fn $name:ident()<'src> -> $out:ty $body:block) => {
        fn $name<'src>(
        ) -> impl Parser<'src, &'src str, $out, extra::Err<Rich<'src, char>>> + Clone
        $body
    };
}

trait SpannedParser<'src, I, O, E>
where
    I: Input<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
    fn spanned(self) -> impl Parser<'src, I, Spanned<O>, E> + Clone;
}

impl<'src, T, I, O, E> SpannedParser<'src, I, O, E> for T
where
    T: Parser<'src, I, O, E> + Clone,
    I: Input<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
    fn spanned(self) -> impl Parser<'src, I, Spanned<O>, E> + Clone {
        self.map_with(|it, e| (it, e.span()))
    }
}

parser_fn! {
    fn label()<'src> -> Token<'src> {
        just('"')
            .ignore_then(
                choice((
                    none_of(r#""\"#).to_slice(),
                    just('\\').then(any()).to_slice(),
                ))
                .repeated()
                .to_slice(),
            )
            .then(choice((
                just('"').map(|_| None),
                end().map_with(|_, extra| Some(extra.span())),
            )))
            .validate(|(label, reached_eof), _, emitter| {
                if let Some(reached_eof) = reached_eof {
                    emitter.emit(<Rich<_> as Error<'src, &'src str>>::expected_found(
                        [Some('"'.into())],
                        None,
                        reached_eof,
                    ))
                }
                label
            })
            .map(TokenKind::Label)
            .spanned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing() {
        let source = r#""hello world"#;
        let source = label().parse(source);
        dbg!(source);
        panic!()
    }
}
