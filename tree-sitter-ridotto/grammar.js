module.exports = grammar({
  name: 'ridotto',

  word: $ => $.ident,
  extras: $ => [
    /\s+/,
    seq('#', /.*/, '\n'),
  ],

  rules: {
    source_file: $ => repeat(choice($.typeDecl, $.func, $.klass)),

    //comment: $ => seq('#', /.*/, '\n'),
    //docComment: $ => seq('##', /.*/, '\n'),

    _typeArgs: $ => seq($.typeExpr, repeat(seq(',', $.typeExpr))),
    typeExpr: $ => seq($.typeName, optional(seq('[', $._typeArgs, ']'))),
    typeName: $ => seq($.ident, repeat(seq('.', $.ident))),

    typeDecl: $ => seq(
      'type',
      field('typeName', $.ident),
      optional(choice(
        field('typeInner', $.typeInner),
        field('typeAlias', seq('=', $.typeExpr)),
      )),
    ),

    typeInner: $ => seq('{',
      field('typeInnerProperties', repeat($.typeAnnotated)),
      field('typeInnerVariants', repeat(seq(
        field('variantName', $.ident),
        field('variant', optional($.typeInner)),
      ))),
      field('methods', repeat($.func)),
    '}'),

    typeAnnotated: $ => seq(
      field('name', $.ident),
      ':',
      field('type', $.typeExpr),
    ),

    funcHead: $ => seq(
      repeat(choice('async', 'const', 'export', 'builtin')), 'func',
      field('name', $.ident),
      '(', field('args', optional(seq(
        $.typeAnnotated,
        repeat(seq(',', $.typeAnnotated)),
      ))), ')',
      field('return', optional(seq('->', $.typeExpr))),
    ),

    func: $ => seq($.funcHead, '{', '}'),

    klass: $ => 'class',

    ident: $ => /[a-zA-Z_]+[a-zA-Z0-9_]*/,
  },
});
