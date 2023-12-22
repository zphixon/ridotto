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

    typeExpr: $ => choice(
      $.normalType,
      $.funcType,
      $.refType,
    ),

    normalType: $ => seq(
      field('name', $.typeName),
      field('instantiate', optional(_wrap(BRACKETS, $.typeExpr))),
    ),

    funcType: $ => _wrap(
      PARENS,
      seq(
        field('args', optional($.typeExpr)),
        '->',
        field('return', optional($.typeExpr)),
      )
    ),

    refType: $ => seq('&', $.typeExpr),

    typeName: $ => _listNoTrail('.', $.ident),
    typeArgs: $ => _list(',', $.typeExpr),

    typeDecl: $ => seq(
      'type',
      field('name', $.ident),
      optional($.innerOrAlias),
    ),

    innerOrAlias: $ => choice(
      $.typeInner,
      field('alias', seq('=', $.typeExpr)),
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
      field('type', $.typeExpr),
    ),

    funcHead: $ => seq(
      field('mods', repeat(choice('async', 'const', 'export', 'builtin'))),
      'func',
      field('name', $.ident),
      field('args', _wrap(PARENS, optional(_list(',', $.typeAnnotated)))),
      field('return', optional(seq('->', $.typeExpr))),
    ),

    func: $ => seq($.funcHead, field('body', $.block)),

    block: $ => _wrap(BRACES, repeat($.stmt)),

    stmt: $ => seq(
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
      field('value', $.expr),
    ),

    expr: $ => choice(
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

    paren: $ => _wrap(PARENS, $.expr),

    dotAccess: $ => seq($.ident, repeat1(seq('.', $.ident))),

    // keep string tokens for operators
    unary: $ => prec(30, choice(
      seq(field('op', '-'), $.expr),
      seq(field('op', '+'), $.expr),
      seq(field('op', '!'), $.expr),
      seq(field('op', '&'), $.expr),
      seq(field('op', '~'), $.expr),
      seq(field('op', '@'), $.expr),
      seq(field('op', '^'), $.expr),
      seq(field('op', 'await'), $.expr),
      seq(field('op', 'yield'), $.expr),
    )),

    binary: $ => choice(
      $.call,
      $.arrayIndex,
      prec.left(20, seq(
        field('lhs', $.expr),
        field('op', choice('*', '/', '%')),
        field('rhs', $.expr))
      ),
      prec.left(19, seq(
        field('lhs', $.expr),
        field('op', choice('+', '-')),
        field('rhs', $.expr))
      ),
      prec.left(18, seq(
        field('lhs', $.expr),
        field('op', choice('^', '&', '|', '<<', '>>')),
        field('rhs', $.expr))
      ),
      prec.left(17, seq(
        field('lhs', $.expr),
        field('op', choice('>', '<', '>=', '<=', '==', '!=', '<>', 'in')),
        field('rhs', $.expr))
      ),
      prec.left(16, seq(
        field('lhs', $.expr),
        field('op', choice('and', '&&')),
        field('rhs', $.expr))
      ),
      prec.left(15, seq(
        field('lhs', $.expr),
        field('op', choice('or', '||')),
        field('rhs', $.expr))
      ),
    ),

    arrayIndex: $ => prec(40, seq(
      field('array', $.expr),
      field('offset', _wrap(BRACKETS, $.expr)),
    )),

    call: $ => prec(40, seq(
      field('callee', $.expr),
      field('args', _wrap(PARENS, optional(_list(',', $.expr)))),
    )),

    ifExpr: $ => seq(
      'if',
      field('cond', $.expr),
      field('true', $.block),
      'else',
      field('false', $.block),
    ),

    ifStmt: $ => seq(
      'if',
      field('cond', $.expr),
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
