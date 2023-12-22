const PARENS = ['(', ')'];
const BRACKETS = ['[', ']'];
const BRACES = ['{', '}'];

const _list = (sep, rule) => seq(rule, repeat(seq(sep, rule)), optional(sep));
const _listMaybeSep = (sep, rule) => seq(rule, repeat(seq(optional(sep), rule)), optional(sep));
const _listNoTrail = (sep, rule) => seq(rule, repeat(seq(sep, rule)));
const _wrap = (w, rule) => seq(w[0], rule, w[1]);

module.exports = grammar({
  name: 'ridotto',

  word: $ => $.ident,
  extras: $ => [
    /\s+/,
    $.comment,
    $.docComment,
  ],

  conflicts: $ => [[$.ifExpr, $.ifStmt]],

  rules: {
    source_file: $ => repeat(choice($.typeDecl, $.func, $.klass)),

    comment: $ => seq('#', /.*/, '\n'),
    docComment: $ => seq('##', /.*/, '\n'),

    typeExpr: $ => $._typeExpr,
    _typeExpr: $ => choice(
      $.normalType,
      $.funcType,
      $.refType,
    ),

    normalType: $ => seq(
      field('name', $.typeName),
      field('instantiate', optional(_wrap(BRACKETS, $._typeExpr))),
    ),

    funcType: $ => _wrap(
      PARENS,
      seq(
        field('args', optional($._typeExpr)),
        '->',
        field('return', optional($._typeExpr)),
      )
    ),

    refType: $ => seq('&', $._typeExpr),

    typeName: $ => _listNoTrail('.', $.ident),
    typeArgs: $ => _list(',', $._typeExpr),

    typeDecl: $ => seq(
      'type',
      field('name', $.ident),
      optional($.innerOrAlias),
    ),

    innerOrAlias: $ => choice(
      $.typeInner,
      field('alias', seq('=', $._typeExpr)),
    ),

    typeInner: $ => _wrap(BRACES,
      seq(
        field('property', optional(_listMaybeSep(',', $.typeAnnotated))),
        field('variant', optional(_listMaybeSep(',', $.typeVariant))),
        field('method', repeat($.func)),
      ),
    ),

    typeVariant: $ => seq(
      field('name', $.ident),
      optional($.innerOrAlias),
    ),

    typeAnnotated: $ => seq(
      field('name', $.ident),
      ':',
      field('type', $._typeExpr),
    ),

    funcHead: $ => $._funcHead,
    _funcHead: $ => seq(
      field('mods', repeat(choice('async', 'const', 'export', 'builtin'))),
      'func',
      field('name', $.ident),
      field('args', _wrap(PARENS, optional(_list(',', $.typeAnnotated)))),
      field('return', optional(seq('->', $._typeExpr))),
    ),

    func: $ => seq($._funcHead, field('body', $.block)),

    block: $ => _wrap(BRACES, repeat($._stmt)),

    stmt: $ => $._stmt,
    _stmt: $ => seq(
      choice(
        $.binding,
        $.ifStmt,
        $.expr,
      ),
      repeat(','),
    ),

    binding: $ => seq(
      'let',
      field('name', $.ident),
      '=',
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
      $.paren,
      $.block,
      $.ifExpr,
    ),

    paren: $ => _wrap(PARENS, $._expr),

    dotAccess: $ => seq($.ident, repeat1(seq('.', $.ident))),

    // keep string tokens for operators
    unary: $ => prec(30, choice(
      seq(field('op', '-'), $._expr),
      seq(field('op', '+'), $._expr),
      seq(field('op', '!'), $._expr),
      seq(field('op', '&'), $._expr),
      seq(field('op', '~'), $._expr),
      seq(field('op', '@'), $._expr),
      seq(field('op', '^'), $._expr),
      seq(field('op', 'await'), $._expr),
      seq(field('op', 'yield'), $._expr),
    )),

    binary: $ => choice(
      $.call,
      $.arrayIndex,
      prec.left(20, seq(
        field('lhs', $._expr),
        field('op', choice('*', '/', '%')),
        field('rhs', $._expr))
      ),
      prec.left(19, seq(
        field('lhs', $._expr),
        field('op', choice('+', '-')),
        field('rhs', $._expr))
      ),
      prec.left(18, seq(
        field('lhs', $._expr),
        field('op', choice('^', '&', '|', '<<', '>>')),
        field('rhs', $._expr))
      ),
      prec.left(17, seq(
        field('lhs', $._expr),
        field('op', choice('>', '<', '>=', '<=', '==', '!=', '<>', 'in')),
        field('rhs', $._expr))
      ),
      prec.left(16, seq(
        field('lhs', $._expr),
        field('op', choice('and', '&&')),
        field('rhs', $._expr))
      ),
      prec.left(15, seq(
        field('lhs', $._expr),
        field('op', choice('or', '||')),
        field('rhs', $._expr))
      ),
    ),

    arrayIndex: $ => prec(40, seq(
      field('array', $._expr),
      field('offset', _wrap(BRACKETS, $._expr)),
    )),

    call: $ => prec(40, seq(
      field('callee', $._expr),
      field('args', _wrap(PARENS, optional(_list(',', $._expr)))),
    )),

    ifExpr: $ => seq(
      'if',
      field('cond', $._expr),
      field('true', $.block),
      'else',
      field('false', $.block),
    ),

    ifStmt: $ => seq(
      'if',
      field('cond', $._expr),
      field('true', $.block),
      optional(seq('else', field('false', $.block))),
    ),

    klass: $ => 'class',

    ident: $ => /[a-zA-Z_]+[a-zA-Z0-9_]*/,
    number: $ => /(\+|-)?[0-9]+(\.[0-9]+)?([eE][0-9]+)?/,
    string: $ => /'(\\.|[^'])*'/s,
    bool: $ => /true|false/,
  },
});
