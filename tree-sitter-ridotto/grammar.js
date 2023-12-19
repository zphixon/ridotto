module.exports = grammar({
  name: 'ridotto',

  word: $ => $.ident,

  rules: {
    source_file: $ => repeat(choice($.type, $.func, $.klass)),

    typeExpr: $ => seq(
      $.typeName,
      optional(seq(
        '[', $.typeArgs, ']',
      )),
    ),

    typeName: $ => seq(
      $.ident,
      repeat(seq('.', $.ident)),
    ),

    typeArgs: $ => seq($.typeExpr, repeat(seq(',', $.typeExpr))),

    type: $ => seq(
      'type',
      field('name', $.ident),
      optional(seq('{', '}')),
    ),

    funcHead: $ => seq(
      repeat(choice('async', 'const', 'export', 'builtin')),
      'func',
      field('name', $.ident),
      '(', ')',
      field('return', optional(seq('->', $.typeExpr))),
    ),

    func: $ => seq(
      $.funcHead,
      '{', '}',
    ),

    klass: $ => 'class',

    ident: $ => /[a-zA-Z_]+[a-zA-Z0-9_]*/,
  }
});
