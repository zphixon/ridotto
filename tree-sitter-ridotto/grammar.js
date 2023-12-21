module.exports = grammar({
  name: 'ridotto',

  word: $ => $.ident,
  extras: $ => [
    /\s+/,
    $._comment,
    $.docComment,
  ],

  rules: {
    source_file: $ => repeat(choice($.typeDecl, $.func, $.klass)),

    _lparen: $ => '(',
    _rparen: $ => ')',
    _lbrack: $ => '[',
    _rbrack: $ => ']',
    _lbrace: $ => '{',
    _rbrace: $ => '}',
    _comma: $ => ',',
    _period: $ => '.',
    _eq: $ => '=',
    _colon: $ => ':',
    _arrow: $ => '->',
    _pound: $ => '#',
    _poundPound: $ => '##',
    _newline: $ => '\n',
    _typeKw: $ => 'type',
    _asyncKw: $ => 'async',
    _constKw: $ => 'const',
    _exportKw: $ => 'export',
    _builtinKw: $ => 'builtin',
    _funcKw: $ => 'func',
    _classKw: $ => 'class',
    _returnKw: $ => 'return',
    _breakKw: $ => 'break',
    _continueKw: $ => 'continue',
    _letKw: $ => 'let',

    _comment: $ => seq($._pound, /.*/, $._newline),
    docComment: $ => seq($._poundPound, /.*/, $._newline),

    _typeArgs: $ => seq($.typeExpr, repeat(seq($._comma, $.typeExpr))),
    typeExpr: $ => seq(
      field('name', $.typeName),
      field('instantiate', optional(seq($._lbrack, $._typeArgs, $._rbrack))),
    ),
    typeName: $ => seq($.ident, repeat(seq($._period, $.ident))),

    typeDecl: $ => seq(
      $._typeKw,
      field('name', $.ident),
      optional($._innerOrAlias),
    ),

    _innerOrAlias: $ => choice(
      $._typeInner,
      field('alias', seq($._eq, $.typeExpr)),
    ),

    _typeInner: $ => seq($._lbrace,
      field('property', repeat($.typeAnnotated)),
      field('variant', repeat($.typeVariant)),
      field('method', repeat($.func)),
    $._rbrace),

    typeVariant: $ => seq(
      field('name', $.ident),
      optional($._innerOrAlias),
    ),

    typeAnnotated: $ => seq(
      field('name', $.ident),
      $._colon,
      field('type', $.typeExpr),
    ),

    funcHead: $ => seq(
      repeat(choice($._asyncKw, $._constKw, $._exportKw, $._builtinKw)), $._funcKw,
      field('name', $.ident),
      $._lparen, field('args', optional(seq(
        $.typeAnnotated,
        repeat(seq($._comma, $.typeAnnotated)),
      ))), $._rparen,
      field('return', optional(seq($._arrow, $.typeExpr))),
    ),

    func: $ => seq($.funcHead, field('body', $.block)),

    block: $ => seq($._lbrace, repeat($._stmt), $._rbrace),

    _stmt: $ => choice(
      $.binding,
    ),

    binding: $ => seq(
      $._letKw,
      field('name', $.ident),
      $._eq,
      field('value', $._expr),
    ),

    expr: $ => $._expr,
    _expr: $ => choice(
      $.ident,
      $.dotAccess,
      $.number,
      $.string,
      $.bool,
      $.unary,
      $.binary,
      $._paren,
    ),

    _paren: $ => seq($._lparen, $._expr, $._rparen),

    dotAccess: $ => seq($.ident, repeat1(seq($._period, $.ident))),

    // keep string tokens for operators
    unary: $ => prec(30, choice(
      seq(field('op', '-'), $._expr),
      seq(field('op', '+'), $._expr),
      seq(field('op', '!'), $._expr),
      seq(field('op', '&'), $._expr),
    )),

    binary: $ => choice(
      $.arrayIndex,
      prec.left(20, seq(
        field('lhs', $._expr),
        field('op', choice('^', '&', '|', '<<', '>>')),
        field('rhs', $._expr))
      ),
      prec.left(15, seq(
        field('lhs', $._expr),
        field('op', choice('*', '/')),
        field('rhs', $._expr))
      ),
      prec.left(10, seq(
        field('lhs', $._expr),
        field('op', choice('+', '-')),
        field('rhs', $._expr))
      ),
    ),

    arrayIndex: $ => prec(40, seq(
      field('array', $._expr),
      $._lbrack,
      field('offset', $._expr),
      $._rbrack,
    )),

    klass: $ => $._classKw,

    ident: $ => /[a-zA-Z_]+[a-zA-Z0-9_]*/,
    number: $ => /(\+|-)?[0-9]+(\.[0-9]+)?([eE][0-9]+)?/,
    string: $ => /'(\\.|[^'])*'/s,
    bool: $ => /true|false/,
  },
});
