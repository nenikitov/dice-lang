mod token;

use logos::Logos;

fn main() {
    let file = std::fs::read_to_string(
        std::env::args()
            .nth(1)
            .expect("First argument should be passed"),
    )
    .expect("File is readable");

    for result in token::Token::lexer(&file) {
        match result {
            Ok(token) => println!("{:?}", token),
            Err(e) => panic!("some error occurred: {:?}", e),
        }
    }
}
