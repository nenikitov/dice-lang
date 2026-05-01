use lrlex::CTLexerBuilder;

fn main() {
    CTLexerBuilder::new()
        .lexer_in_src_dir("grammar/die.l")
        .unwrap()
        .lrpar_config(|ctp| ctp.grammar_in_src_dir("grammar/die.y").unwrap())
        .build()
        .unwrap();
}
