use crate::scan::{Scanner, Token, TokenType};
use std::cell::Cell;

#[derive(Debug)]
pub struct Tree<'src> {
    pub type_: TreeType,
    pub children: Vec<Child<'src>>,
}

#[derive(Debug)]
pub enum Child<'src> {
    Token(Token<'src>),
    Tree(Tree<'src>),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TreeType {
    ErrorTree,

    Module,
    Name,

    TypeDecl,
    TypeDeclUnit,
    TypeDeclAlias,

    TypeVariant,
    TypeField,
    TypeMethod,

    TypeExpr,
    TypeExprInstantiation,
}

impl Tree<'_> {
    pub fn name(&self) -> Option<Token> {
        self.children
            .iter()
            .flat_map(|child| match child {
                Child::Tree(Tree {
                    type_: TreeType::Name,
                    children,
                }) => match &children[..] {
                    [Child::Token(token)] => Some(*token),
                    _ => None,
                },
                _ => None,
            })
            .next()
    }
}

enum Event {
    Open { type_: TreeType },
    Close,
    Advance,
}

struct MarkOpened {
    index: usize,
}

struct MarkClosed {
    index: usize,
}

pub struct Parser<'src> {
    scanner: Scanner<'src>,
    fuel: Cell<u32>,
    events: Vec<Event>,
}

impl<'src> Parser<'src> {
    pub fn parse(src: &'src str) -> Tree<'src> {
        let mut parser = Parser {
            scanner: Scanner::new(src),
            fuel: Cell::new(256),
            events: vec![],
        };
        module(&mut parser);
        parser.build_tree()
    }

    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            type_: TreeType::ErrorTree,
        });
        tracing::trace!("open {}", mark.index);
        mark
    }

    fn close(&mut self, mark: MarkOpened, type_: TreeType) -> MarkClosed {
        self.events[mark.index] = Event::Open { type_ };
        self.events.push(Event::Close);
        tracing::trace!("close {:?} {}", type_, mark.index);
        MarkClosed { index: mark.index }
    }

    fn open_before(&mut self, mark: MarkClosed) -> MarkOpened {
        let mark = MarkOpened { index: mark.index };
        self.events.insert(
            mark.index,
            Event::Open {
                type_: TreeType::ErrorTree,
            },
        );
        mark
    }

    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel.set(256);
        self.events.push(Event::Advance);
        let next = self.scanner.next_token();
        tracing::trace!("advance {:?}", next.type_);
    }

    fn eof(&mut self) -> bool {
        self.scanner.peek_token().type_ == TokenType::Eof
    }

    fn nth(&mut self, lookahead: usize) -> Token {
        if self.fuel.get() == 0 {
            panic!("parser is stuck. this is a bug");
        }

        self.fuel.set(self.fuel.get() - 1);
        self.scanner.lookahead(lookahead)
    }

    fn at(&mut self, type_: TokenType) -> bool {
        self.nth(0).type_ == type_
    }

    fn eat(&mut self, type_: TokenType) -> bool {
        if self.at(type_) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, type_: TokenType) {
        if self.eat(type_) {
            return;
        }

        tracing::error!("expected {type_:?}");
    }

    fn advance_with_error(&mut self, error: &str) {
        let mark = self.open();
        tracing::error!("{error}");
        self.advance();
        self.close(mark, TreeType::ErrorTree);
    }

    fn build_tree(self) -> Tree<'src> {
        let Parser {
            mut scanner,
            mut events,
            ..
        } = self;

        tracing::info!("build tree");
        assert!(matches!(events.pop(), Some(Event::Close)));

        let mut stack = Vec::new();

        for event in events {
            match event {
                Event::Open { type_ } => {
                    stack.push(Tree {
                        type_,
                        children: Vec::with_capacity(2),
                    });
                    tracing::trace!("open {:?} {:?}", type_, stack);
                }

                Event::Close => {
                    let tree = stack.pop().unwrap();
                    let type_ = tree.type_;
                    stack.last_mut().unwrap().children.push(Child::Tree(tree));
                    tracing::trace!("close {:?} {:?}", type_, stack);
                }

                Event::Advance => {
                    let token = scanner.next_token();
                    stack.last_mut().unwrap().children.push(Child::Token(token));
                    tracing::trace!("advance {:?} {:?}", token.type_, stack);
                }
            }
        }

        assert!(stack.len() == 1);
        assert!(scanner.next_token().type_ == TokenType::Eof);

        stack.pop().unwrap()
    }
}

mod x {
    use super::*;
    use crate::scan::Pos;

    #[test]
    fn tree() {
        let named = Tree {
            type_: TreeType::TypeDeclUnit,
            children: vec![Child::Tree(Tree {
                type_: TreeType::Name,
                children: vec![Child::Token(Token {
                    type_: TokenType::UpperIdent,
                    lexeme: "Unit",
                    pos: Pos::Builtin,
                })],
            })],
        };

        let _tree: Tree<'static> = Tree {
            type_: TreeType::Module,
            children: vec![
                Child::Tree(Tree {
                    type_: TreeType::TypeDeclUnit,
                    children: vec![Child::Token(Token {
                        type_: TokenType::UpperIdent,
                        lexeme: "Unit",
                        pos: Pos::Builtin,
                    })],
                }),
                Child::Tree(Tree {
                    type_: TreeType::TypeDecl,
                    children: vec![Child::Tree(Tree {
                        type_: TreeType::TypeVariant,
                        children: vec![
                            Child::Tree(Tree {
                                type_: TreeType::Name,
                                children: vec![Child::Token(Token {
                                    type_: TokenType::UpperIdent,
                                    lexeme: "Bit",
                                    pos: Pos::Builtin,
                                })],
                            }),
                            Child::Tree(Tree {
                                type_: TreeType::TypeVariant,
                                children: vec![Child::Token(Token {
                                    type_: TokenType::UpperIdent,
                                    lexeme: "Zero",
                                    pos: Pos::Builtin,
                                })],
                            }),
                            Child::Tree(Tree {
                                type_: TreeType::TypeVariant,
                                children: vec![Child::Token(Token {
                                    type_: TokenType::UpperIdent,
                                    lexeme: "One",
                                    pos: Pos::Builtin,
                                })],
                            }),
                        ],
                    })],
                }),
            ],
        };
    }
}

fn module<'src>(parser: &mut Parser<'src>) {
    tracing::debug!("module");
    let mark = parser.open();
    while !parser.eof() {
        if parser.at(TokenType::Type) {
            type_(parser)
        } else {
            parser.advance_with_error("expected an item");
        }
    }
    parser.close(mark, TreeType::Module);
}

fn type_<'src>(parser: &mut Parser<'src>) {
    tracing::debug!("type");
    assert!(parser.at(TokenType::Type));
    let mark = parser.open();

    parser.expect(TokenType::Type);
    parser.expect(TokenType::UpperIdent);

    if parser.at(TokenType::LeftBrace) {
        // TODO type X { ... }
    } else if parser.at(TokenType::LeftParen) {
        // TODO type X(...)
    } else if parser.at(TokenType::Equal) {
        // TODO type X = ...
    } else {
        tracing::trace!("type decl unit");
        parser.close(mark, TreeType::TypeDeclUnit);
    }
}
