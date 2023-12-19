#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 40
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 29
#define ALIAS_COUNT 0
#define TOKEN_COUNT 18
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 2
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 5

enum {
  sym_ident = 1,
  anon_sym_LBRACK = 2,
  anon_sym_RBRACK = 3,
  anon_sym_DOT = 4,
  anon_sym_COMMA = 5,
  anon_sym_type = 6,
  anon_sym_LBRACE = 7,
  anon_sym_RBRACE = 8,
  anon_sym_async = 9,
  anon_sym_const = 10,
  anon_sym_export = 11,
  anon_sym_builtin = 12,
  anon_sym_func = 13,
  anon_sym_LPAREN = 14,
  anon_sym_RPAREN = 15,
  anon_sym_DASH_GT = 16,
  sym_klass = 17,
  sym_source_file = 18,
  sym_typeExpr = 19,
  sym_typeName = 20,
  sym_typeArgs = 21,
  sym_type = 22,
  sym_funcHead = 23,
  sym_func = 24,
  aux_sym_source_file_repeat1 = 25,
  aux_sym_typeName_repeat1 = 26,
  aux_sym_typeArgs_repeat1 = 27,
  aux_sym_funcHead_repeat1 = 28,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_ident] = "ident",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_DOT] = ".",
  [anon_sym_COMMA] = ",",
  [anon_sym_type] = "type",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_async] = "async",
  [anon_sym_const] = "const",
  [anon_sym_export] = "export",
  [anon_sym_builtin] = "builtin",
  [anon_sym_func] = "func",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_DASH_GT] = "->",
  [sym_klass] = "klass",
  [sym_source_file] = "source_file",
  [sym_typeExpr] = "typeExpr",
  [sym_typeName] = "typeName",
  [sym_typeArgs] = "typeArgs",
  [sym_type] = "type",
  [sym_funcHead] = "funcHead",
  [sym_func] = "func",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_typeName_repeat1] = "typeName_repeat1",
  [aux_sym_typeArgs_repeat1] = "typeArgs_repeat1",
  [aux_sym_funcHead_repeat1] = "funcHead_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_ident] = sym_ident,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_type] = anon_sym_type,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_async] = anon_sym_async,
  [anon_sym_const] = anon_sym_const,
  [anon_sym_export] = anon_sym_export,
  [anon_sym_builtin] = anon_sym_builtin,
  [anon_sym_func] = anon_sym_func,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [sym_klass] = sym_klass,
  [sym_source_file] = sym_source_file,
  [sym_typeExpr] = sym_typeExpr,
  [sym_typeName] = sym_typeName,
  [sym_typeArgs] = sym_typeArgs,
  [sym_type] = sym_type,
  [sym_funcHead] = sym_funcHead,
  [sym_func] = sym_func,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_typeName_repeat1] = aux_sym_typeName_repeat1,
  [aux_sym_typeArgs_repeat1] = aux_sym_typeArgs_repeat1,
  [aux_sym_funcHead_repeat1] = aux_sym_funcHead_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_ident] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_type] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_async] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_const] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_export] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_builtin] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_func] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [sym_klass] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_typeExpr] = {
    .visible = true,
    .named = true,
  },
  [sym_typeName] = {
    .visible = true,
    .named = true,
  },
  [sym_typeArgs] = {
    .visible = true,
    .named = true,
  },
  [sym_type] = {
    .visible = true,
    .named = true,
  },
  [sym_funcHead] = {
    .visible = true,
    .named = true,
  },
  [sym_func] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_typeName_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_typeArgs_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_funcHead_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_name = 1,
  field_return = 2,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_name] = "name",
  [field_return] = "return",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 1},
  [3] = {.index = 2, .length = 3},
  [4] = {.index = 5, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 1},
  [1] =
    {field_name, 2},
  [2] =
    {field_name, 1},
    {field_return, 4},
    {field_return, 5},
  [5] =
    {field_name, 2},
    {field_return, 5},
    {field_return, 6},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 37,
  [38] = 38,
  [39] = 39,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(2);
      if (lookahead == '(') ADVANCE(9);
      if (lookahead == ')') ADVANCE(10);
      if (lookahead == ',') ADVANCE(6);
      if (lookahead == '-') ADVANCE(1);
      if (lookahead == '.') ADVANCE(5);
      if (lookahead == '[') ADVANCE(3);
      if (lookahead == ']') ADVANCE(4);
      if (lookahead == '{') ADVANCE(7);
      if (lookahead == '}') ADVANCE(8);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(12);
      END_STATE();
    case 1:
      if (lookahead == '>') ADVANCE(11);
      END_STATE();
    case 2:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 3:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 4:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(sym_ident);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(13);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(12);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(sym_ident);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(13);
      END_STATE();
    default:
      return false;
  }
}

static bool ts_lex_keywords(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (lookahead == 'a') ADVANCE(1);
      if (lookahead == 'b') ADVANCE(2);
      if (lookahead == 'c') ADVANCE(3);
      if (lookahead == 'e') ADVANCE(4);
      if (lookahead == 'f') ADVANCE(5);
      if (lookahead == 't') ADVANCE(6);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == 's') ADVANCE(7);
      END_STATE();
    case 2:
      if (lookahead == 'u') ADVANCE(8);
      END_STATE();
    case 3:
      if (lookahead == 'l') ADVANCE(9);
      if (lookahead == 'o') ADVANCE(10);
      END_STATE();
    case 4:
      if (lookahead == 'x') ADVANCE(11);
      END_STATE();
    case 5:
      if (lookahead == 'u') ADVANCE(12);
      END_STATE();
    case 6:
      if (lookahead == 'y') ADVANCE(13);
      END_STATE();
    case 7:
      if (lookahead == 'y') ADVANCE(14);
      END_STATE();
    case 8:
      if (lookahead == 'i') ADVANCE(15);
      END_STATE();
    case 9:
      if (lookahead == 'a') ADVANCE(16);
      END_STATE();
    case 10:
      if (lookahead == 'n') ADVANCE(17);
      END_STATE();
    case 11:
      if (lookahead == 'p') ADVANCE(18);
      END_STATE();
    case 12:
      if (lookahead == 'n') ADVANCE(19);
      END_STATE();
    case 13:
      if (lookahead == 'p') ADVANCE(20);
      END_STATE();
    case 14:
      if (lookahead == 'n') ADVANCE(21);
      END_STATE();
    case 15:
      if (lookahead == 'l') ADVANCE(22);
      END_STATE();
    case 16:
      if (lookahead == 's') ADVANCE(23);
      END_STATE();
    case 17:
      if (lookahead == 's') ADVANCE(24);
      END_STATE();
    case 18:
      if (lookahead == 'o') ADVANCE(25);
      END_STATE();
    case 19:
      if (lookahead == 'c') ADVANCE(26);
      END_STATE();
    case 20:
      if (lookahead == 'e') ADVANCE(27);
      END_STATE();
    case 21:
      if (lookahead == 'c') ADVANCE(28);
      END_STATE();
    case 22:
      if (lookahead == 't') ADVANCE(29);
      END_STATE();
    case 23:
      if (lookahead == 's') ADVANCE(30);
      END_STATE();
    case 24:
      if (lookahead == 't') ADVANCE(31);
      END_STATE();
    case 25:
      if (lookahead == 'r') ADVANCE(32);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(anon_sym_func);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_type);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(anon_sym_async);
      END_STATE();
    case 29:
      if (lookahead == 'i') ADVANCE(33);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(sym_klass);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_const);
      END_STATE();
    case 32:
      if (lookahead == 't') ADVANCE(34);
      END_STATE();
    case 33:
      if (lookahead == 'n') ADVANCE(35);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_export);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_builtin);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_ident] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_type] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_async] = ACTIONS(1),
    [anon_sym_const] = ACTIONS(1),
    [anon_sym_export] = ACTIONS(1),
    [anon_sym_builtin] = ACTIONS(1),
    [anon_sym_func] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [sym_klass] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(38),
    [sym_type] = STATE(3),
    [sym_funcHead] = STATE(37),
    [sym_func] = STATE(3),
    [aux_sym_source_file_repeat1] = STATE(3),
    [aux_sym_funcHead_repeat1] = STATE(7),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_type] = ACTIONS(5),
    [anon_sym_async] = ACTIONS(7),
    [anon_sym_const] = ACTIONS(7),
    [anon_sym_export] = ACTIONS(7),
    [anon_sym_builtin] = ACTIONS(7),
    [anon_sym_func] = ACTIONS(9),
    [sym_klass] = ACTIONS(11),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 8,
    ACTIONS(13), 1,
      ts_builtin_sym_end,
    ACTIONS(15), 1,
      anon_sym_type,
    ACTIONS(21), 1,
      anon_sym_func,
    ACTIONS(24), 1,
      sym_klass,
    STATE(7), 1,
      aux_sym_funcHead_repeat1,
    STATE(37), 1,
      sym_funcHead,
    STATE(2), 3,
      sym_type,
      sym_func,
      aux_sym_source_file_repeat1,
    ACTIONS(18), 4,
      anon_sym_async,
      anon_sym_const,
      anon_sym_export,
      anon_sym_builtin,
  [30] = 8,
    ACTIONS(5), 1,
      anon_sym_type,
    ACTIONS(9), 1,
      anon_sym_func,
    ACTIONS(27), 1,
      ts_builtin_sym_end,
    ACTIONS(29), 1,
      sym_klass,
    STATE(7), 1,
      aux_sym_funcHead_repeat1,
    STATE(37), 1,
      sym_funcHead,
    STATE(2), 3,
      sym_type,
      sym_func,
      aux_sym_source_file_repeat1,
    ACTIONS(7), 4,
      anon_sym_async,
      anon_sym_const,
      anon_sym_export,
      anon_sym_builtin,
  [60] = 2,
    ACTIONS(33), 1,
      anon_sym_LBRACE,
    ACTIONS(31), 8,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_async,
      anon_sym_const,
      anon_sym_export,
      anon_sym_builtin,
      anon_sym_func,
      sym_klass,
  [74] = 1,
    ACTIONS(35), 8,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_async,
      anon_sym_const,
      anon_sym_export,
      anon_sym_builtin,
      anon_sym_func,
      sym_klass,
  [85] = 1,
    ACTIONS(37), 8,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_async,
      anon_sym_const,
      anon_sym_export,
      anon_sym_builtin,
      anon_sym_func,
      sym_klass,
  [96] = 3,
    ACTIONS(41), 1,
      anon_sym_func,
    STATE(9), 1,
      aux_sym_funcHead_repeat1,
    ACTIONS(39), 4,
      anon_sym_async,
      anon_sym_const,
      anon_sym_export,
      anon_sym_builtin,
  [109] = 3,
    ACTIONS(45), 1,
      anon_sym_DOT,
    STATE(8), 1,
      aux_sym_typeName_repeat1,
    ACTIONS(43), 4,
      anon_sym_LBRACK,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_LBRACE,
  [122] = 3,
    ACTIONS(51), 1,
      anon_sym_func,
    STATE(9), 1,
      aux_sym_funcHead_repeat1,
    ACTIONS(48), 4,
      anon_sym_async,
      anon_sym_const,
      anon_sym_export,
      anon_sym_builtin,
  [135] = 3,
    ACTIONS(55), 1,
      anon_sym_DOT,
    STATE(8), 1,
      aux_sym_typeName_repeat1,
    ACTIONS(53), 4,
      anon_sym_LBRACK,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_LBRACE,
  [148] = 3,
    ACTIONS(55), 1,
      anon_sym_DOT,
    STATE(10), 1,
      aux_sym_typeName_repeat1,
    ACTIONS(57), 4,
      anon_sym_LBRACK,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_LBRACE,
  [161] = 1,
    ACTIONS(43), 5,
      anon_sym_LBRACK,
      anon_sym_RBRACK,
      anon_sym_DOT,
      anon_sym_COMMA,
      anon_sym_LBRACE,
  [169] = 4,
    ACTIONS(59), 1,
      sym_ident,
    STATE(14), 1,
      sym_typeName,
    STATE(18), 1,
      sym_typeExpr,
    STATE(34), 1,
      sym_typeArgs,
  [182] = 2,
    ACTIONS(61), 1,
      anon_sym_LBRACK,
    ACTIONS(63), 3,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_LBRACE,
  [191] = 1,
    ACTIONS(65), 3,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_LBRACE,
  [197] = 3,
    ACTIONS(67), 1,
      anon_sym_RBRACK,
    ACTIONS(69), 1,
      anon_sym_COMMA,
    STATE(19), 1,
      aux_sym_typeArgs_repeat1,
  [207] = 3,
    ACTIONS(59), 1,
      sym_ident,
    STATE(14), 1,
      sym_typeName,
    STATE(23), 1,
      sym_typeExpr,
  [217] = 3,
    ACTIONS(69), 1,
      anon_sym_COMMA,
    ACTIONS(71), 1,
      anon_sym_RBRACK,
    STATE(16), 1,
      aux_sym_typeArgs_repeat1,
  [227] = 3,
    ACTIONS(73), 1,
      anon_sym_RBRACK,
    ACTIONS(75), 1,
      anon_sym_COMMA,
    STATE(19), 1,
      aux_sym_typeArgs_repeat1,
  [237] = 3,
    ACTIONS(59), 1,
      sym_ident,
    STATE(14), 1,
      sym_typeName,
    STATE(30), 1,
      sym_typeExpr,
  [247] = 3,
    ACTIONS(59), 1,
      sym_ident,
    STATE(14), 1,
      sym_typeName,
    STATE(26), 1,
      sym_typeExpr,
  [257] = 2,
    ACTIONS(78), 1,
      anon_sym_LBRACE,
    ACTIONS(80), 1,
      anon_sym_DASH_GT,
  [264] = 1,
    ACTIONS(73), 2,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [269] = 2,
    ACTIONS(82), 1,
      anon_sym_LBRACE,
    ACTIONS(84), 1,
      anon_sym_DASH_GT,
  [276] = 1,
    ACTIONS(86), 1,
      anon_sym_RPAREN,
  [280] = 1,
    ACTIONS(88), 1,
      anon_sym_LBRACE,
  [284] = 1,
    ACTIONS(90), 1,
      sym_ident,
  [288] = 1,
    ACTIONS(92), 1,
      sym_ident,
  [292] = 1,
    ACTIONS(94), 1,
      anon_sym_LPAREN,
  [296] = 1,
    ACTIONS(96), 1,
      anon_sym_LBRACE,
  [300] = 1,
    ACTIONS(98), 1,
      anon_sym_RPAREN,
  [304] = 1,
    ACTIONS(100), 1,
      anon_sym_RBRACE,
  [308] = 1,
    ACTIONS(102), 1,
      sym_ident,
  [312] = 1,
    ACTIONS(104), 1,
      anon_sym_RBRACK,
  [316] = 1,
    ACTIONS(106), 1,
      anon_sym_RBRACE,
  [320] = 1,
    ACTIONS(108), 1,
      anon_sym_LPAREN,
  [324] = 1,
    ACTIONS(110), 1,
      anon_sym_LBRACE,
  [328] = 1,
    ACTIONS(112), 1,
      ts_builtin_sym_end,
  [332] = 1,
    ACTIONS(114), 1,
      sym_ident,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 30,
  [SMALL_STATE(4)] = 60,
  [SMALL_STATE(5)] = 74,
  [SMALL_STATE(6)] = 85,
  [SMALL_STATE(7)] = 96,
  [SMALL_STATE(8)] = 109,
  [SMALL_STATE(9)] = 122,
  [SMALL_STATE(10)] = 135,
  [SMALL_STATE(11)] = 148,
  [SMALL_STATE(12)] = 161,
  [SMALL_STATE(13)] = 169,
  [SMALL_STATE(14)] = 182,
  [SMALL_STATE(15)] = 191,
  [SMALL_STATE(16)] = 197,
  [SMALL_STATE(17)] = 207,
  [SMALL_STATE(18)] = 217,
  [SMALL_STATE(19)] = 227,
  [SMALL_STATE(20)] = 237,
  [SMALL_STATE(21)] = 247,
  [SMALL_STATE(22)] = 257,
  [SMALL_STATE(23)] = 264,
  [SMALL_STATE(24)] = 269,
  [SMALL_STATE(25)] = 276,
  [SMALL_STATE(26)] = 280,
  [SMALL_STATE(27)] = 284,
  [SMALL_STATE(28)] = 288,
  [SMALL_STATE(29)] = 292,
  [SMALL_STATE(30)] = 296,
  [SMALL_STATE(31)] = 300,
  [SMALL_STATE(32)] = 304,
  [SMALL_STATE(33)] = 308,
  [SMALL_STATE(34)] = 312,
  [SMALL_STATE(35)] = 316,
  [SMALL_STATE(36)] = 320,
  [SMALL_STATE(37)] = 324,
  [SMALL_STATE(38)] = 328,
  [SMALL_STATE(39)] = 332,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [15] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(28),
  [18] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(7),
  [21] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(39),
  [24] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(2),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [31] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 2, .production_id = 1),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 4, .production_id = 1),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_func, 3),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_typeName_repeat1, 2),
  [45] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_typeName_repeat1, 2), SHIFT_REPEAT(27),
  [48] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_funcHead_repeat1, 2), SHIFT_REPEAT(9),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_funcHead_repeat1, 2),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_typeName, 2),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_typeName, 1),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [63] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_typeExpr, 1),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_typeExpr, 4),
  [67] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_typeArgs, 2),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_typeArgs, 1),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_typeArgs_repeat1, 2),
  [75] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_typeArgs_repeat1, 2), SHIFT_REPEAT(17),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_funcHead, 4, .production_id = 1),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [82] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_funcHead, 5, .production_id = 2),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_funcHead, 6, .production_id = 3),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [96] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_funcHead, 7, .production_id = 4),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [112] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_ridotto(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .keyword_lex_fn = ts_lex_keywords,
    .keyword_capture_token = sym_ident,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
