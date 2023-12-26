const PARENS = ['(', ')'];
const SQUARES = ['[', ']'];
const CURLIES = ['{', '}'];

const _list = (sep, rule) => seq(rule, repeat(seq(sep, rule)), optional(sep));
const _listMaybeSep = (sep, rule) => seq(rule, optional(sep), repeat(seq(rule, optional(sep))));
const _listNoTrail = (sep, rule) => seq(rule, repeat(seq(sep, rule)));
const _wrap = (w, rule) => seq(w[0], rule, w[1]);

module.exports = grammar({
  name: 'ridotto',

  word: $ => $.anyIdent,
  extras: $ => [
    /\s+/,
    $.comment,
    $.docComment,
  ],

  conflicts: $ => [
    [$._pattern, $.typeName],
    [$._expr, $.typeName],
    [$.destructure, $.variant],
    [$.typeName, $.dotAccess],
    [$.paren, $.tuple],
    [$.lowerIdent, $.upperIdent],
  ],

  rules: {
    source_file: $ => repeat(choice($.typeDecl, $.func, $.klass)),

    comment: $ => seq('#', /.*/, '\n'),
    docComment: $ => seq('##', /.*/, '\n'),

    typeExpr: $ => $._typeExpr,
    _typeExpr: $ => choice(
      $.normalType,
      $.funcType,
      $.refType,
      $.tupleType,
    ),

    normalType: $ => seq(
      field('name', $.typeName),
      field('instantiate', optional(_wrap(SQUARES, $._typeExpr))),
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

    tupleType: $ => _wrap(PARENS, _list(',', $._typeExpr)),

    typeName: $ => _listNoTrail('.', $.upperIdent),

    typeDecl: $ => seq(
      'type',
      field('name', $.normalType),
      optional($._innerOrAlias),
    ),

    _innerOrAlias: $ => choice(
      $.typeInner,
      $.typeAlias,
    ),

    typeInner: $ => _wrap(CURLIES,
      seq(
        field('property', optional(_listMaybeSep(',', $.typeAnnotated))),
        field('variant', optional(_listMaybeSep(',', $.typeVariant))),
        field('method', repeat($.func)),
      ),
    ),

    typeAlias: $ => seq('=', $._typeExpr),

    typeVariant: $ => seq(
      field('name', $.upperIdent),
      optional($._innerOrAlias),
    ),

    typeAnnotated: $ => seq(
      field('name', $.lowerIdent),
      ':',
      field('type', $._typeExpr),
    ),

    funcHead: $ => $._funcHead,
    _funcHead: $ => seq(
      field('mods', repeat(choice('async', 'const', 'export', 'builtin'))),
      'func',
      field('name', $.lowerIdent),
      field('args', _wrap(PARENS, optional(_list(',', $.typeAnnotated)))),
      field('return', optional(seq('->', $._typeExpr))),
    ),

    func: $ => seq($._funcHead, field('body', $.block)),

    block: $ => _wrap(CURLIES, repeat($._stmt)),

    stmt: $ => $._stmt,
    _stmt: $ => seq(
      choice(
        $.binding,
        $.expr,
      ),
      repeat(','),
    ),

    binding: $ => seq(
      'let',
      field('pattern', $._pattern),
      '=',
      field('value', $._expr),
    ),

    expr: $ => $._expr,
    _expr: $ => choice(
      $.lowerIdent,
      $.dotAccess,
      $.number,
      $.string,
      $.bool,
      $.unary,
      $.binary,
      $.paren,
      $.tuple,
      $.ifExpr,
      $.matchExpr,
      $.instantiate,
    ),

    paren: $ => _wrap(PARENS, $._expr),
    tuple: $ => _wrap(PARENS, _list(',', $._expr)),

    dotAccess: $ => seq($.lowerIdent, repeat1(seq('.', choice($.lowerIdent, /[0-9]+/)))),

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
      field('offset', _wrap(SQUARES, $._expr)),
    )),

    call: $ => prec(40, seq(
      field('callee', $._expr),
      field('args', _wrap(PARENS, optional(_list(',', $._expr)))),
    )),

    ifExpr: $ => seq(
      'if',
      field('cond', $._expr),
      field('true', $.block),
      optional(seq('else', field('false', $.block))),
    ),

    matchExpr: $ => seq(
      'match',
      field('disc', $._expr),
      field('arms', _wrap(CURLIES, _listMaybeSep(',', $.matchArm))),
    ),

    matchArm: $ => seq(
      field('pattern', $._pattern),
      field('guard', optional(seq('if', $._expr))),
      field('body', $.block),
    ),

    instantiate: $ => seq(
      field('type', $.typeName),
      _wrap(CURLIES,
        _list(',', seq(
          field('name', $.lowerIdent),
          optional(seq(':', field('value', $._expr))),
        ))
      ),
    ),

    pattern: $ => $._pattern,
    _pattern: $ => choice(
      $.wildcard,
      $.lowerIdent,
      $.alternate,
      $.destructure,
      $.variant,
      $.tuplePattern,
    ),

    wildcard: $ => '_',

    alternate: $ => prec.left(50, seq(
      field('lhs', $._pattern),
      '|',
      field('rhs', $._pattern),
    )),

    destructure: $ => seq(
      field('type', $.typeName),
      field('fields', _wrap(CURLIES, seq(
        _list(',', $.lowerIdent),
        optional($.rest),
      ))),
    ),

    rest: $ => '..',

    variant: $ => $.typeName,

    tuplePattern: $ => _wrap(PARENS, _list(',', $._pattern)),

    klass: $ => 'class',

    anyIdent: $ => /[a-zA-Z]+[a-zA-Z0-9_]*/,
    lowerIdent: $ => /[a-z]+[a-zA-Z0-9_]*/,
    upperIdent: $ => /[A-Z_]+[a-zA-Z0-9_]*/,

    number: $ => /(\+|-)?[0-9]+(\.[0-9]+)?([eE][0-9]+)?/,
    string: $ => /'(\\.|[^'])*'/s,
    bool: $ => /true|false/,
  },
});
