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

    _comment: $ => seq($._pound, /.*/, $._newline),
    docComment: $ => seq($._poundPound, /.*/, $._newline),

    _typeArgs: $ => seq($.typeExpr, repeat(seq($._comma, $.typeExpr))),
    typeExpr: $ => seq(
      field('name', $.typeName),
      field('instantiate', optional(seq($._lbrack, $._typeArgs, $._rbrack))),
    ),
    typeName: $ => seq($.ident, repeat(seq($._period, $.ident))),

    typeDecl: $ => seq(
      'type',
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
      repeat(choice('async', 'const', 'export', 'builtin')), 'func',
      field('name', $.ident),
      $._lparen, field('args', optional(seq(
        $.typeAnnotated,
        repeat(seq($._comma, $.typeAnnotated)),
      ))), $._rparen,
      field('return', optional(seq($._arrow, $.typeExpr))),
    ),

    func: $ => seq($.funcHead, field('body', seq($._lbrace, $._rbrace))),

    klass: $ => 'class',

    ident: $ => /[a-zA-Z_]+[a-zA-Z0-9_]*/,
  },
});
