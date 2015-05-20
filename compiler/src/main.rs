use std::fs::File;

mod token;
mod lexer;

#[cfg_attr(test, allow(dead_code))]
fn main() {
    let mut f = File::open("foo.tamanduang").unwrap();
    let mut lex = lexer::Lexer::<File>::new(&mut f);

    let t = lex.nextToken();
    if let Ok(ref t) = t {
        println!("Category: {:?}", t.category());
    }
    println!("{:?}", t);
}
