%{
#include "microParser.hpp"
#include <math.h>
%}

INTLITERAL      [0-9]+
FLOATLITERAL    [0-9]*\.+[0-9]+
STRINGLITERAL   (["'])(?:(?=(\\?))\2.)*\1
ID              [a-z]+[a-z0-9]*

%%
	
{INTLITERAL}+  {
           // printf( "Token Type: INTLITERAL\n Value: (%d)\n", yytext );
           return INTLITERAL
            }

{FLOATLITERAL}+ {
            //printf( "Token Type: FLOATLITERAL\n Value: %s (%g)\n", yytext );
	   return FLOATLITERAL
            }


PROGRAM {

        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return PROGRAM;
}

BEGIN {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return _BEGIN;
}

END {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return END;
}

FUNCTION {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return FUNCTION;
}

READ {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return READ;
}

WRITE {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return WRITE;
}

IF {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return IF;
}

ELSE {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return ELSE;
}

ENDIF {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return ENDIF;
}

WHILE {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return WHILE;
}

ENDWHILE {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return ENDWHILE;
}

RETURN {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return RETURN;
}

INT {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        yylval.str_val = strdup(yytext);
        return INT;
}
IN {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return IN;
}

RANGE {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return RANGE;
}
VOID {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return VOID;
}

STRING {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return STRING;
}

FLOAT {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        yylval.str_val = strdup(yytext);
        return FLOAT;
}

TRUE {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return TRUE;
}

FALSE {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return FALSE;
}

FOR {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return FOR;
}

ENDFOR {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return ENDFOR;
}

CONTINUE {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return CONTINUE;
}

BREAK {
        //printf ("Token Type: KEYWORD\nValue: %s\n", yytext);
        return BREAK;
}


{ID} {
        //printf ("Token Type: IDENTIFIER\nValue: %s\n", yytext);
        yylval.str_val = strdup(yytext);
        return IDENTIFIER;
}


"=" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return ASSIGN;
}

"+" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return ADD;
}

"-" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return SUB;
}

"*" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return MUL;
}

"/" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return DIV;
}

"=" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return EQL;
}

"!=" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return NEQL;
}

"<" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return LESS;
}

">" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return GRT;
}

"(" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return ST_BRACE;
}

")" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return END_BRACE;
}
";" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return SEMICOLON;
}
"," {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return COMMA;
}
"<=" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return LEQ;
}

">=" {
        //printf ("Token Type: OPERATOR\nValue: %s\n", yytext);
        return GEQ;
}

%%
