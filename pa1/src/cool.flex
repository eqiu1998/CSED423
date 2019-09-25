 /*
  *  The scanner definition for COOL.
  */

 /*
  *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
  *  output, so headers and global definitions are placed here to be visible
  * to the code in the file.  Don't remove anything that was here initially
  */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int commentLevel = 0;

%}

%option noyywrap

/*
 * Define names for regular expressions here.
 */

digit       [0-9]
symbol      "+"|"-"|"*"|"/"|"~"|"<"|"="|"("|")"|"{"|"}"|";"|":"|"."|","|"@"
typeid      [A-Z][a-zA-Z0-9_]*
objid		    [a-z][a-zA-Z0-9_]*
whitespace  [ \r\t\f\v]

 /*** State Definitions ***/
%x CMT STR DEAD_STR

%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */

 /*** Keywords ***/
(?i:class)      return CLASS;
(?i:else)       return ELSE;
(?i:fi)         return FI;
(?i:if)			    return IF;
(?i:in)			    return IN;
(?i:inherits)		return INHERITS;
(?i:let)        return LET;
(?i:loop)		    return LOOP;
(?i:pool)		    return POOL;
(?i:then)		    return THEN;
(?i:while)		  return WHILE;
(?i:case)		    return CASE;
(?i:esac)		    return ESAC;
(?i:of)			    return OF;
(?i:new)		    return NEW;
(?i:isvoid)		  return ISVOID;
(?i:not)		    return NOT;
(t)(?i:rue)		  {
                  cool_yylval.boolean = true;
                  return BOOL_CONST;
                }
(f)(?i:alse)	  {
                  cool_yylval.boolean = false;
                  return BOOL_CONST;
                }

"=>"			      return DARROW;
"<-"			      return ASSIGN;
"<="			      return LE;

 /*** Single-chars ***/
{symbol}        return int(yytext[0]);

 /*** Comments ***/
"(*"		{
          ++commentLevel;
          BEGIN(CMT);				
        }
"*)"    {
          cool_yylval.error_msg = "Unmatched *)";
				  return ERROR;
        }

(--)(.)*

<CMT>"(*"   ++commentLevel;
<CMT>"*)"   {
                  if(!--commentLevel) BEGIN(INITIAL);
                  if(commentLevel < 0) {
                    cool_yylval.error_msg = "Unmatched *)";
					          commentLevel=0;
                    return ERROR;
                  }
                }
<CMT>\n     ++curr_lineno;
<CMT>(.|{whitespace}+)
<CMT><<EOF>>	{
                    BEGIN(INITIAL);
                    if(commentLevel>0){
                      cool_yylval.error_msg = "EOF in comment";
                      commentLevel=0;
                      return ERROR;
                    }
                  }

 /*** Strings ***/
"\""            {
                  string_buf_ptr = string_buf;
                  BEGIN(STR);
                }
<STR>"\""    {
                  if(string_buf_ptr - string_buf > MAX_STR_CONST-1){
                    *string_buf = '\0';
                    cool_yylval.error_msg = "String constant too long";
                    BEGIN(INITIAL);
                    return ERROR;
                  }
                  *string_buf_ptr = '\0';
                  cool_yylval.symbol = stringtable.add_string(string_buf);
                  BEGIN(INITIAL);
                  return STR_CONST;
                }

<STR><<EOF>>		{
                    cool_yylval.error_msg = "EOF in string constant";
                    BEGIN(INITIAL);
                    return ERROR;
                  }
<STR>\0		{
                *string_buf = '\0';
                cool_yylval.error_msg = "String contains null character.";
                BEGIN(DEAD_STR);
                return ERROR;
              }
<STR>\n		{     ++curr_lineno;
                *string_buf = '\0';
                cool_yylval.error_msg = "Unterminated string constant";
                BEGIN(INITIAL);
                return ERROR;
              }

<STR>\\b       *string_buf_ptr++ = '\b';
<STR>\\t       *string_buf_ptr++ = '\t';
<STR>"\\n"     *string_buf_ptr++ = '\n';
<STR>"\\\n"    *string_buf_ptr++ = '\n'; ++curr_lineno;
<STR>\\f       *string_buf_ptr++ = '\f';
<STR>\\[^\0]	 *string_buf_ptr++ = yytext[1];
<STR>.         *string_buf_ptr++ = yytext[0];

<DEAD_STR>"\\\""
<DEAD_STR>\"		BEGIN(INITIAL);
<DEAD_STR>\n    ++curr_lineno; BEGIN(INITIAL);
<DEAD_STR>.

 /*** Identifiers ***/
{digit}+		    {
                  cool_yylval.symbol = inttable.add_string(yytext);
                  return INT_CONST;
                }
{typeid}		    {
                  cool_yylval.symbol = idtable.add_string(yytext);
                  return TYPEID;
                }
{objid} 	      {
                  cool_yylval.symbol = idtable.add_string(yytext);
                  return OBJECTID;
                }

 /*** whitespace ***/
\n		++curr_lineno;
{whitespace}+

 /*** invalid ***/
.		{
			cool_yylval.error_msg = yytext;
			return ERROR;
		}

%%
