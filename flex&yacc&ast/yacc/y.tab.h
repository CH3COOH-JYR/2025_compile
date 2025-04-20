/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 27 "sysy.y"
{
    int int_val;
    char* str_val;
    char* id_val;
}
/* Line 1529 of yacc.c.  */
#line 135 "y.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

