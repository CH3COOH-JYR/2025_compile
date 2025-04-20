/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IDENFR = 258,
     INTCON = 259,
     STRCON = 260,
     MAINTK = 261,
     CONSTTK = 262,
     INTTK = 263,
     BREAKTK = 264,
     CONTINUETK = 265,
     IFTK = 266,
     ELSETK = 267,
     NOT = 268,
     AND = 269,
     OR = 270,
     WHILETK = 271,
     GETINTTK = 272,
     PRINTFTK = 273,
     PLUS = 274,
     MINU = 275,
     VOIDTK = 276,
     MULT = 277,
     DIV = 278,
     MOD = 279,
     LSS = 280,
     LEQ = 281,
     GRE = 282,
     GEQ = 283,
     EQL = 284,
     NEQ = 285,
     ASSIGN = 286,
     SEMICN = 287,
     COMMA = 288,
     LPARENT = 289,
     RPARENT = 290,
     LBRACK = 291,
     RBRACK = 292,
     LBRACE = 293,
     RBRACE = 294,
     RETURNTK = 295
   };
#endif
/* Tokens.  */
#define IDENFR 258
#define INTCON 259
#define STRCON 260
#define MAINTK 261
#define CONSTTK 262
#define INTTK 263
#define BREAKTK 264
#define CONTINUETK 265
#define IFTK 266
#define ELSETK 267
#define NOT 268
#define AND 269
#define OR 270
#define WHILETK 271
#define GETINTTK 272
#define PRINTFTK 273
#define PLUS 274
#define MINU 275
#define VOIDTK 276
#define MULT 277
#define DIV 278
#define MOD 279
#define LSS 280
#define LEQ 281
#define GRE 282
#define GEQ 283
#define EQL 284
#define NEQ 285
#define ASSIGN 286
#define SEMICN 287
#define COMMA 288
#define LPARENT 289
#define RPARENT 290
#define LBRACK 291
#define RBRACK 292
#define LBRACE 293
#define RBRACE 294
#define RETURNTK 295




/* Copy the first part of user declarations.  */
#line 1 "sysy.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// 输出文件
FILE *yylexout = NULL;  // 词法分析输出
FILE *yyparsout = NULL; // 语法分析输出

extern int yylex();
extern FILE* yyin;
extern char* yytext;

void yyerror(const char* s);

// 输出语法成分的函数
void output_grammar_component(const char* name) {
    if (yyparsout) {
        fprintf(yyparsout, "<%s>\n", name);
    }
}

// 全局变量，用于追踪是否是最后一次CompUnit规则的应用
int is_final_comp_unit = 0;


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 27 "sysy.y"
{
    int int_val;
    char* str_val;
    char* id_val;
}
/* Line 193 of yacc.c.  */
#line 208 "y.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 221 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  14
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   299

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  41
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  39
/* YYNRULES -- Number of rules.  */
#define YYNRULES  108
/* YYNRULES -- Number of states.  */
#define YYNSTATES  216

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   295

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    13,    15,    17,    22,
      28,    31,    35,    39,    44,    48,    53,    55,    58,    62,
      67,    70,    74,    78,    83,    86,    90,    92,    95,    99,
     104,   106,   109,   113,   118,   121,   125,   131,   138,   144,
     151,   157,   159,   163,   166,   171,   177,   181,   186,   190,
     193,   195,   198,   200,   202,   207,   209,   212,   214,   220,
     228,   234,   237,   240,   243,   247,   254,   261,   267,   270,
     274,   276,   278,   280,   283,   287,   292,   296,   298,   300,
     302,   304,   308,   313,   316,   318,   320,   322,   324,   328,
     330,   334,   338,   342,   344,   348,   352,   354,   358,   362,
     366,   370,   372,   376,   380,   382,   386,   388,   392
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      42,     0,    -1,    43,    -1,    55,    -1,    42,    43,    -1,
      42,    55,    -1,    44,    -1,    50,    -1,     7,     8,    46,
      32,    -1,     7,     8,    46,    45,    32,    -1,    33,    46,
      -1,    45,    33,    46,    -1,     3,    31,    48,    -1,     3,
      47,    31,    48,    -1,    36,    79,    37,    -1,    47,    36,
      79,    37,    -1,    79,    -1,    38,    39,    -1,    38,    48,
      39,    -1,    38,    48,    49,    39,    -1,    33,    48,    -1,
      49,    33,    48,    -1,     8,    52,    32,    -1,     8,    52,
      51,    32,    -1,    33,    52,    -1,    51,    33,    52,    -1,
       3,    -1,     3,    47,    -1,     3,    31,    53,    -1,     3,
      47,    31,    53,    -1,    64,    -1,    38,    39,    -1,    38,
      53,    39,    -1,    38,    53,    54,    39,    -1,    33,    53,
      -1,    54,    33,    53,    -1,    21,     3,    34,    35,    59,
      -1,    21,     3,    34,    56,    35,    59,    -1,     8,     3,
      34,    35,    59,    -1,     8,     3,    34,    56,    35,    59,
      -1,     8,     6,    34,    35,    59,    -1,    57,    -1,    56,
      33,    57,    -1,     8,     3,    -1,     8,     3,    36,    37,
      -1,     8,     3,    36,    37,    58,    -1,    36,    79,    37,
      -1,    58,    36,    79,    37,    -1,    38,    60,    39,    -1,
      38,    39,    -1,    61,    -1,    60,    61,    -1,    43,    -1,
      62,    -1,    66,    31,    64,    32,    -1,    32,    -1,    64,
      32,    -1,    59,    -1,    11,    34,    65,    35,    62,    -1,
      11,    34,    65,    35,    62,    12,    62,    -1,    16,    34,
      65,    35,    62,    -1,     9,    32,    -1,    10,    32,    -1,
      40,    32,    -1,    40,    64,    32,    -1,    66,    31,    17,
      34,    35,    32,    -1,    18,    34,     5,    63,    35,    32,
      -1,    18,    34,     5,    35,    32,    -1,    33,    64,    -1,
      63,    33,    64,    -1,    74,    -1,    78,    -1,     3,    -1,
       3,    67,    -1,    36,    64,    37,    -1,    67,    36,    64,
      37,    -1,    34,    64,    35,    -1,    66,    -1,    69,    -1,
       4,    -1,    68,    -1,     3,    34,    35,    -1,     3,    34,
      72,    35,    -1,    71,    70,    -1,    19,    -1,    20,    -1,
      13,    -1,    64,    -1,    72,    33,    64,    -1,    70,    -1,
      73,    22,    70,    -1,    73,    23,    70,    -1,    73,    24,
      70,    -1,    73,    -1,    74,    19,    73,    -1,    74,    20,
      73,    -1,    74,    -1,    75,    25,    74,    -1,    75,    26,
      74,    -1,    75,    27,    74,    -1,    75,    28,    74,    -1,
      75,    -1,    76,    29,    75,    -1,    76,    30,    75,    -1,
      76,    -1,    77,    14,    76,    -1,    77,    -1,    78,    15,
      77,    -1,    74,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    51,    51,    52,    53,    54,    61,    62,    66,    67,
      70,    71,    75,    76,    80,    81,    85,    86,    87,    88,
      91,    92,    96,    97,   100,   101,   105,   106,   107,   108,
     112,   113,   114,   115,   118,   119,   123,   124,   125,   126,
     127,   131,   132,   136,   137,   138,   141,   142,   146,   147,
     150,   151,   154,   155,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   175,   176,
     180,   184,   188,   189,   192,   193,   197,   198,   199,   202,
     206,   207,   208,   209,   212,   213,   214,   218,   219,   223,
     224,   225,   226,   230,   231,   232,   236,   237,   238,   239,
     240,   244,   245,   246,   250,   251,   255,   256,   260
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENFR", "INTCON", "STRCON", "MAINTK",
  "CONSTTK", "INTTK", "BREAKTK", "CONTINUETK", "IFTK", "ELSETK", "NOT",
  "AND", "OR", "WHILETK", "GETINTTK", "PRINTFTK", "PLUS", "MINU", "VOIDTK",
  "MULT", "DIV", "MOD", "LSS", "LEQ", "GRE", "GEQ", "EQL", "NEQ", "ASSIGN",
  "SEMICN", "COMMA", "LPARENT", "RPARENT", "LBRACK", "RBRACK", "LBRACE",
  "RBRACE", "RETURNTK", "$accept", "CompUnit", "Decl", "ConstDecl",
  "ConstDefList", "ConstDef", "ArrayDims", "ConstInitVal",
  "ConstInitValList", "VarDecl", "VarDefList", "VarDef", "InitVal",
  "InitValList", "FuncDef", "FuncFParams", "FuncFParam",
  "FuncFParamArrayDims", "Block", "BlockItems", "BlockItem", "Stmt",
  "PrintfParams", "Exp", "Cond", "LVal", "ArraySubscripts", "PrimaryExp",
  "Number", "UnaryExp", "UnaryOp", "FuncRParams", "MulExp", "AddExp",
  "RelExp", "EqExp", "LAndExp", "LOrExp", "ConstExp", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    41,    42,    42,    42,    42,    43,    43,    44,    44,
      45,    45,    46,    46,    47,    47,    48,    48,    48,    48,
      49,    49,    50,    50,    51,    51,    52,    52,    52,    52,
      53,    53,    53,    53,    54,    54,    55,    55,    55,    55,
      55,    56,    56,    57,    57,    57,    58,    58,    59,    59,
      60,    60,    61,    61,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    63,    63,
      64,    65,    66,    66,    67,    67,    68,    68,    68,    69,
      70,    70,    70,    70,    71,    71,    71,    72,    72,    73,
      73,    73,    73,    74,    74,    74,    75,    75,    75,    75,
      75,    76,    76,    76,    77,    77,    78,    78,    79
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     2,     1,     1,     4,     5,
       2,     3,     3,     4,     3,     4,     1,     2,     3,     4,
       2,     3,     3,     4,     2,     3,     1,     2,     3,     4,
       1,     2,     3,     4,     2,     3,     5,     6,     5,     6,
       5,     1,     3,     2,     4,     5,     3,     4,     3,     2,
       1,     2,     1,     1,     4,     1,     2,     1,     5,     7,
       5,     2,     2,     2,     3,     6,     6,     5,     2,     3,
       1,     1,     1,     2,     3,     4,     3,     1,     1,     1,
       1,     3,     4,     2,     1,     1,     1,     1,     3,     1,
       3,     3,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     1,     3,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     0,     2,     6,     7,     3,     0,
      26,     0,     0,     0,     1,     4,     5,     0,     0,     0,
       0,     0,    27,     0,    22,     0,     0,     0,     0,     0,
       8,     0,     0,    72,    79,    86,    84,    85,     0,     0,
      28,    30,    77,    80,    78,    89,     0,    93,    70,     0,
       0,     0,    41,   108,     0,     0,     0,     0,    26,    24,
      23,     0,     0,     0,     0,    12,    16,     0,    10,     9,
       0,     0,     0,    73,     0,    31,     0,    83,     0,     0,
       0,     0,     0,    43,     0,    38,     0,     0,    14,    29,
       0,    40,    25,    36,     0,    17,     0,    13,    11,    81,
      87,     0,     0,     0,    76,     0,    32,     0,    90,    91,
      92,    94,    95,     0,     0,     0,     0,     0,     0,     0,
      55,    49,     0,    52,    57,     0,    50,    53,     0,    77,
      42,    39,    15,    37,     0,    18,     0,     0,    82,    74,
       0,    34,     0,    33,    44,    61,    62,     0,     0,     0,
      63,     0,    48,    51,    56,     0,    20,     0,    19,    88,
      75,    35,     0,    45,     0,    96,   101,   104,   106,    71,
       0,     0,    64,     0,     0,    21,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    54,    46,     0,    58,    97,    98,    99,   100,
     102,   103,   105,   107,    60,    68,    67,     0,     0,     0,
      47,     0,    69,    66,    65,    59
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,   123,     6,    32,    18,    22,    65,   136,     7,
      26,    12,    40,   107,     8,    51,    52,   163,   124,   125,
     126,   127,   190,    41,   164,    42,    73,    43,    44,    45,
      46,   101,    47,    48,   166,   167,   168,   169,    66
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -161
static const yytype_int16 yypact[] =
{
      47,     7,    86,    18,   104,  -161,  -161,  -161,  -161,    38,
      44,    29,    97,    59,  -161,  -161,  -161,   -25,   123,   221,
       5,   265,    28,    53,  -161,   102,   135,     8,   223,    54,
    -161,    38,   149,    60,  -161,  -161,  -161,  -161,   265,    19,
    -161,  -161,  -161,  -161,  -161,  -161,   265,   111,   132,   106,
      78,    75,  -161,   132,    99,   221,   265,    78,    55,  -161,
    -161,   102,    78,    80,   210,  -161,  -161,   223,  -161,  -161,
      38,   119,   265,   105,   137,  -161,   -13,  -161,   265,   265,
     265,   265,   265,   108,    63,  -161,   171,    78,  -161,  -161,
     147,  -161,  -161,  -161,    78,  -161,    -9,  -161,  -161,  -161,
    -161,    91,   157,   265,  -161,   221,  -161,    -8,  -161,  -161,
    -161,   111,   111,   158,   102,   164,   165,   166,   170,   177,
    -161,  -161,   243,  -161,  -161,   167,  -161,  -161,   184,   189,
    -161,  -161,  -161,  -161,   223,  -161,    -4,   265,  -161,  -161,
     161,  -161,   221,  -161,   185,  -161,  -161,   265,   265,   217,
    -161,   196,  -161,  -161,  -161,   247,  -161,   223,  -161,  -161,
    -161,  -161,   265,   202,   197,   132,    93,   159,   231,   220,
     218,   112,  -161,   224,   222,  -161,   215,   265,   199,   265,
     265,   265,   265,   265,   265,   265,   265,   199,   265,   233,
     113,   235,  -161,  -161,   234,   260,   132,   132,   132,   132,
      93,    93,   159,   231,  -161,  -161,  -161,   265,   241,   242,
    -161,   199,  -161,  -161,  -161,  -161
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -161,  -161,    52,  -161,  -161,   -22,   259,   -57,  -161,  -161,
    -161,   -11,   -36,  -161,   275,   253,   200,  -161,   -45,  -161,
     162,  -160,  -161,   -38,   134,   -80,  -161,  -161,  -161,   -18,
    -161,  -161,   109,   -20,     9,    98,   103,  -161,   -19
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      74,    53,    54,    76,   129,    85,    28,    96,    53,    68,
      97,    21,    91,    49,    59,     9,    49,    93,   195,    89,
     105,    13,    33,    34,   134,   142,   106,   204,    77,   157,
     135,   143,    35,   100,   102,   158,    53,    90,    36,    37,
      50,    17,   131,    62,    53,   129,   128,    53,    98,   133,
      92,   215,     5,    38,     1,     2,    15,    39,    75,    55,
     108,   109,   110,    23,    56,   140,    33,    34,     3,   141,
       1,   114,   115,   116,   117,    19,    35,   156,    20,   118,
      21,   119,    36,    37,   151,    67,    19,   128,    57,    10,
      56,    21,    11,    27,    71,   120,    72,    38,   129,   159,
     175,    84,   121,   122,    14,    58,   161,   129,    86,    83,
      87,     1,     2,    86,    53,    94,    84,   174,   179,   180,
     181,   182,    33,    34,   137,     3,   138,   165,   165,    24,
      25,   129,    35,    78,    79,    80,    88,    53,    36,    37,
     128,   103,    53,   176,   113,   188,   207,   189,   208,   128,
     205,    81,    82,    38,    99,    30,    31,    53,   194,   196,
     197,   198,   199,   165,   165,   165,   165,    60,    61,   212,
      33,    34,   104,   128,     1,   114,   115,   116,   117,    49,
      35,    69,    70,   118,   132,   119,    36,    37,   183,   184,
     111,   112,   200,   201,   139,   144,   145,   146,   160,   120,
     147,    38,    33,    34,   148,    84,   152,   122,   115,   116,
     117,   149,    35,    33,    34,   118,   154,   119,    36,    37,
     155,   162,   171,    35,    33,    34,    33,    34,   172,    36,
      37,   120,   178,    38,    35,   186,    35,    84,   177,   122,
      36,    37,    36,    37,    38,   185,    33,    34,    64,    95,
      33,    34,   193,   187,   192,    38,    35,    38,   191,    39,
      35,    64,    36,    37,   173,   206,    36,    37,    33,    34,
     209,   210,   211,   213,   214,   150,    29,    38,    35,    16,
      63,    38,   170,   202,    36,    37,   130,   153,     0,   203,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    38
};

static const yytype_int16 yycheck[] =
{
      38,    21,    21,    39,    84,    50,    31,    64,    28,    31,
      67,    36,    57,     8,    25,     8,     8,    62,   178,    55,
      33,     3,     3,     4,    33,    33,    39,   187,    46,    33,
      39,    39,    13,    71,    72,    39,    56,    56,    19,    20,
      35,     3,    87,    35,    64,   125,    84,    67,    70,    94,
      61,   211,     0,    34,     7,     8,     4,    38,    39,    31,
      78,    79,    80,    34,    36,   103,     3,     4,    21,   105,
       7,     8,     9,    10,    11,    31,    13,   134,    34,    16,
      36,    18,    19,    20,   122,    31,    31,   125,    35,     3,
      36,    36,     6,    34,    34,    32,    36,    34,   178,   137,
     157,    38,    39,    40,     0,     3,   142,   187,    33,     3,
      35,     7,     8,    33,   134,    35,    38,   155,    25,    26,
      27,    28,     3,     4,    33,    21,    35,   147,   148,    32,
      33,   211,    13,    22,    23,    24,    37,   157,    19,    20,
     178,    36,   162,   162,    36,    33,    33,    35,    35,   187,
     188,    19,    20,    34,    35,    32,    33,   177,   177,   179,
     180,   181,   182,   183,   184,   185,   186,    32,    33,   207,
       3,     4,    35,   211,     7,     8,     9,    10,    11,     8,
      13,    32,    33,    16,    37,    18,    19,    20,    29,    30,
      81,    82,   183,   184,    37,    37,    32,    32,    37,    32,
      34,    34,     3,     4,    34,    38,    39,    40,     9,    10,
      11,    34,    13,     3,     4,    16,    32,    18,    19,    20,
      31,    36,     5,    13,     3,     4,     3,     4,    32,    19,
      20,    32,    35,    34,    13,    15,    13,    38,    36,    40,
      19,    20,    19,    20,    34,    14,     3,     4,    38,    39,
       3,     4,    37,    35,    32,    34,    13,    34,    34,    38,
      13,    38,    19,    20,    17,    32,    19,    20,     3,     4,
      35,    37,    12,    32,    32,    32,    17,    34,    13,     4,
      27,    34,   148,   185,    19,    20,    86,   125,    -1,   186,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    34
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     7,     8,    21,    42,    43,    44,    50,    55,     8,
       3,     6,    52,     3,     0,    43,    55,     3,    46,    31,
      34,    36,    47,    34,    32,    33,    51,    34,    31,    47,
      32,    33,    45,     3,     4,    13,    19,    20,    34,    38,
      53,    64,    66,    68,    69,    70,    71,    73,    74,     8,
      35,    56,    57,    74,    79,    31,    36,    35,     3,    52,
      32,    33,    35,    56,    38,    48,    79,    31,    46,    32,
      33,    34,    36,    67,    64,    39,    53,    70,    22,    23,
      24,    19,    20,     3,    38,    59,    33,    35,    37,    53,
      79,    59,    52,    59,    35,    39,    48,    48,    46,    35,
      64,    72,    64,    36,    35,    33,    39,    54,    70,    70,
      70,    73,    73,    36,     8,     9,    10,    11,    16,    18,
      32,    39,    40,    43,    59,    60,    61,    62,    64,    66,
      57,    59,    37,    59,    33,    39,    49,    33,    35,    37,
      64,    53,    33,    39,    37,    32,    32,    34,    34,    34,
      32,    64,    39,    61,    32,    31,    48,    33,    39,    64,
      37,    53,    36,    58,    65,    74,    75,    76,    77,    78,
      65,     5,    32,    17,    64,    48,    79,    36,    35,    25,
      26,    27,    28,    29,    30,    14,    15,    35,    33,    35,
      63,    34,    32,    37,    79,    62,    74,    74,    74,    74,
      75,    75,    76,    77,    62,    64,    32,    33,    35,    35,
      37,    12,    64,    32,    32,    62
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 5:
#line 54 "sysy.y"
    { 
                // 这里不立即输出CompUnit标记，等到程序结束时再输出
                is_final_comp_unit = 1;
             }
    break;

  case 8:
#line 66 "sysy.y"
    { output_grammar_component("ConstDecl"); }
    break;

  case 9:
#line 67 "sysy.y"
    { output_grammar_component("ConstDecl"); }
    break;

  case 22:
#line 96 "sysy.y"
    { output_grammar_component("VarDecl"); }
    break;

  case 23:
#line 97 "sysy.y"
    { output_grammar_component("VarDecl"); }
    break;

  case 36:
#line 123 "sysy.y"
    { output_grammar_component("FuncDef"); }
    break;

  case 37:
#line 124 "sysy.y"
    { output_grammar_component("FuncDef"); }
    break;

  case 38:
#line 125 "sysy.y"
    { output_grammar_component("FuncDef"); }
    break;

  case 39:
#line 126 "sysy.y"
    { output_grammar_component("FuncDef"); }
    break;

  case 40:
#line 127 "sysy.y"
    { output_grammar_component("MainFuncDef"); }
    break;

  case 48:
#line 146 "sysy.y"
    { output_grammar_component("Block"); }
    break;

  case 49:
#line 147 "sysy.y"
    { output_grammar_component("Block"); }
    break;

  case 54:
#line 159 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 55:
#line 160 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 56:
#line 161 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 57:
#line 162 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 58:
#line 163 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 59:
#line 164 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 60:
#line 165 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 61:
#line 166 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 62:
#line 167 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 63:
#line 168 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 64:
#line 169 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 65:
#line 170 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 66:
#line 171 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;

  case 67:
#line 172 "sysy.y"
    { output_grammar_component("Stmt"); }
    break;


/* Line 1267 of yacc.c.  */
#line 1758 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 263 "sysy.y"


void yyerror(const char* s) {
    fprintf(stderr, "Error: %s\n", s);
}

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <input_file> <output_file>\n", argv[0]);
        return 1;
    }

    FILE *in = fopen(argv[1], "r");
    if (!in) {
        fprintf(stderr, "Error: Could not open input file %s\n", argv[1]);
        return 1;
    }

    yylexout = fopen(argv[2], "w");
    if (!yylexout) {
        fprintf(stderr, "Error: Could not open output file %s\n", argv[2]);
        fclose(in);
        return 1;
    }

    // 语法分析输出也输出到同一个文件
    yyparsout = yylexout;

    yyin = in;
    yyparse();
    
    // 解析完成后，如果设置了is_final_comp_unit标志，输出CompUnit标记
    if (is_final_comp_unit) {
        output_grammar_component("CompUnit");
    }

    fclose(in);
    fclose(yylexout);
    return 0;
} 
