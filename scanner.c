/*
* File Name: scanner.c
* Compiler: MS Visual Studio 2015
* Author: Adam White 040 518 422 James Holmes 040 776 963
* Course: CST 8152 - Compilers, Lab Section: 11/13 (respectively)
* Assignment: 2
* Date: November 30, 2017
* Professor: Sv. Ranev
* Purpose: Utility funtions for scanner
* Function list:
*	char_class()
*	aa_func02()
*	aa_func11()
*	atolh()
*	keword_index()
*	aa_func03()
*	aa_func05()
*	aa_func08()
*	aa_func11()
*	aa_func12()
*	malar_next_token()
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define SEOF 255

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

						 /* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

					   /* No other global variable declarations/definitiond are allowed */

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static char keyword_index(char * kw_lexeme); /*keywords lookup functuion */
static long atolh(char * lexeme); /* converts hexadecimal string to decimal value */

								  /*
								  * Purpose: Initialize the scanner.
								  * Author: S.Ranev
								  * Version: 1.01
								  * Called Functons: b_isempty(buffer*), b_rewind(buffer*), b_clear(buffer*)
								  * Parameters: sc_buf -> a pointer to a buffer used to reset the initilzaion of
								  * the scanner based on the newbuffer pointer.
								  * Return: EXIT_FAILURE on failure, EXIT_SUCCESS on success
								  */
int scanner_init(Buffer * sc_buf) {
	/* Verify buffer is not NULL. */
	if (b_isempty(sc_buf)) return EXIT_FAILURE;
	/* Rewind the buffer incase it is not currently at its startig location. */
	b_rewind(sc_buf);
	b_clear(str_LTBL);
	/* Reset number of lines read. */
	line = 1;
	return EXIT_SUCCESS;
}

/*
* Purpose: Scan through a buffer parsing it as it goes producing various tokens.
* Author: Adam White, James Holmes
* Version: 1.02
* Called Functons: b_mark(), b_getc(), b_getcoffset(), b_retract(), b_reset(), strlen(), b_location(), b_addc, b_free
* Parameters: sc_buf-> pointer to the buffer to scan.
* Returns: Token
* Algorithm: Reads characters 1 by 1 from buffer
* assigns and returns tokens for special cases
* FSM handles other cases by calling accepting functions to return Tokens
*/
Token malar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	int counter;
	while (1) { /* endless loop broken by token returns it will generate a warning */
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));
		c = b_getc(sc_buf);

		/* special cases or token driven processing */
		if (c == ' ' || c == '\t' || c == '\v' || c == '\f') continue; /*non line-terminating whitespace*/
		if (c == '#') { t.code = SCC_OP_T; /*no attribute*/ return t; }/*string concatenation operator*/
																	   /*if (c=='=') {t.code=ASS_OP_T; /*no attribute*//* return t;}*//*assignment operator*/
		if (c == '+') { t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t; }/*plus arithmentic operator*/
		if (c == '-') { t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t; }/*minus arithmentic operator*/
		if (c == '*') { t.code = ART_OP_T; t.attribute.arr_op = MULT; return t; }/*multiplicative arithmentic operator*/
		if (c == '/') { t.code = ART_OP_T; t.attribute.arr_op = DIV; return t; }/*division arithmentic operator*/
		if (c == '=') {
			if (b_getc(sc_buf) == '=') { t.code = REL_OP_T; t.attribute.rel_op = EQ; return t; }/*equality relational operator*/
			else { b_retract(sc_buf); t.code = ASS_OP_T; return t; }/*assignment operator*/
		}
		if (c == '<') {
			if (b_getc(sc_buf) == '>') { t.code = REL_OP_T; t.attribute.rel_op = NE; return t; }/*not equals relational operator*/
			else { b_retract(sc_buf); t.code = REL_OP_T; t.attribute.rel_op = LT; return t; }/*greater than relational operator*/
		}
		if (c == '>') { t.code = REL_OP_T; t.attribute.rel_op = GT; return t; }/*less than relational operator*/
		if (c == '.') {/*captures relational operators as they begin with .*/
			b_mark(sc_buf, b_getcoffset(sc_buf));
			if (b_getc(sc_buf) == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.') {/*if .AND.*/
				t.code = LOG_OP_T; t.attribute.log_op = AND; return t;
			}
			b_reset(sc_buf);
			if (b_getc(sc_buf) == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.') {/*if .OR.*/
				t.code = LOG_OP_T; t.attribute.log_op = OR; return t;
			}
			b_reset(sc_buf);
			t.code = ERR_T;/*if not .AND. or .OR., it's an error*/
			strcpy(t.attribute.err_lex, ".");
			return t;
		}
		if (c == '!') {/*handles comments*/
			lexstart = b_getcoffset(sc_buf);
			if ((c = b_getc(sc_buf)) != '!') {/*creates error token if char following ! isn't !*/
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';
			}
			while (c != '\n' && c != '\r')/*proceed to line terminator*/
				c = b_getc(sc_buf);
			line++;
			if (*b_location(sc_buf, lexstart) != '!') return t;/*this causes a warning C4701: potentially unitialized local variable 't' used, but I can assure you that t will be initialized, it retruns error token created earlier*/
			continue;
		}
		if (c == '"') {/*handles string literals*/
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));
			while ((c = b_getc(sc_buf)) != '"') {/*reads string literal from opening " to closing "*/
				if (c == '\n' || c == '\r') line++;
				if (c == '\0' || c == SEOF) {/*handles invalid string literal characters*/
					lexend = b_getcoffset(sc_buf);
					b_reset(sc_buf);
					b_retract(sc_buf);
					if (lexend - lexstart > 20) {/*error if can't fit token attribute*/
						for (counter = 0; counter < 17; counter++)
							t.attribute.err_lex[counter] = b_getc(sc_buf);
						for (counter; counter < 20; counter++)/*adds ...*/
							t.attribute.err_lex[counter] = '.';
						t.attribute.err_lex[counter] = '\0';
						b_mark(sc_buf, lexend - 1);
						b_reset(sc_buf);
					}
					else {/*error if can fit in token attribute*/
						for (counter = 0; counter < (lexend - lexstart); counter++)
							t.attribute.err_lex[counter] = b_getc(sc_buf);
						t.attribute.err_lex[counter] = '\0';
					}
					t.code = ERR_T;
					return t;
				}
			}
			b_reset(sc_buf);/*if valid string literal, reset*/
			t.attribute.str_offset = b_limit(str_LTBL);/*token attribute*/
			while ((c = b_getc(sc_buf)) != '"') {/*add to string literal table*/
				b_addc(str_LTBL, c);
			}
			b_addc(str_LTBL, '\0');
			t.code = STR_T;
			/*t.attribute.str_offset = lexstart;*/
			return t;
		}
		if (c == '\n' || c == '\r') { line++; continue; }/*line terminators*/
		if (c == '(') { t.code = LPR_T; return t; }/*left parenthesis*/
		if (c == ')') { t.code = RPR_T; return t; }/*right parenthesis*/
		if (c == '{') { t.code = LBR_T; return t; }/*left brace*/
		if (c == '}') { t.code = RBR_T; return t; }/*right brace*/
		if (c == ',') { t.code = COM_T; return t; }/*comma*/
		if (c == ';') { t.code = EOS_T; return t; }/*end of statement*/
		if (c == '\0' || c == SEOF) { t.code = SEOF_T; return t; }/*source end of file*/

		if (isalpha(c) || isdigit(c)) {/*if alphanumeric, run FSM*/
			while (1) {/*Move from state to state until arrive to accepting state*/
				state = get_next_state(state, c, &accept);
				if (accept == NOAS) c = b_getc(sc_buf);
				else break;
			}
			if (as_table[state] == ASWR) b_retract(sc_buf);/*if accepring state with retract, retract*/
			lexend = b_getcoffset(sc_buf);/*offset for end of lexeme*/
			lex_buf = b_allocate((lexend - lexstart) + 1, 0, 'f');/*will hold lexeme*/
			b_reset(sc_buf);
			for (counter = 0; counter < (lexend - lexstart); counter++) {/*add lexeme to lex buffer*/
				b_addc(lex_buf, b_getc(sc_buf));
			}
			b_addc(lex_buf, '\0');

			t = aa_table[state](b_location(lex_buf, 0));/*extract lexeme from lex buffer and send to proper accepting function*/

			b_free(lex_buf);
			return t;
		}
		else { t.code = ERR_T; t.attribute.err_lex[0] = c; t.attribute.err_lex[1] = '\0'; return t; }/*if invalid input char*/

	}//end while(1)
}
/*
* Purpose: Verify the state of the buffer and the next char to determine the next state to feed to.
* Author: S.Ranev
* Version: 1.01
* Called Functons: char_class()
* Parameters: state->
* c->
* accept->
*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}
/*
* Purpose: Takes an input charter which is used to convert to the correct coorosponding integer character code type.
* Author: Adam White
* Version: 1.01
* Called Functons:
* Parameters: c-> the charcter to retrieve its character code type.
* Returns: Integer value pertaining to the characters type.
*/
int char_class(char c) {
	if ((c >= 'a'&&c <= 'w') || (c == 'y' || c == 'z') || (c >= 'G' && c <= 'Z')) return 0;
	if (c >= '1' && c <= '9') return 1;
	if (c == '0') return 2;
	if (c == 'x') return 3;
	if (c >= 'A' && c <= 'F') return 4;
	if (c == '$') return 5;
	if (c == '.') return 6;
	return 7;
}
/*
* Purpose: Pass in a character array to check against Keyword and Variable token parameters. Returns
appropriate token.
* Author: Adam White
* Version: 1.01
* Called Functons: strcopy(), keyword_index(), strlen(), strncpy()
* Parameters: lexeme-> the character buffer used to attempt to produce a Keyword token or variable
identifier token.
* Returns: Appropriate token (Keyword, variable on success or error on error).
* Algorithm: Check if keyword
* Return AVID token if not keyword, otherwise return KW token
*/
Token aa_func02(char lexeme[]) {
	Token t; /* New Token to create and return. */
	char kw_index; /* Used to store the ID value if the Lexeme is a Key Word. */
				   /* Check for NULL lexeme, if true return Error token. */
	/* Check the lexeme against all Keywords. */
	kw_index = keyword_index(lexeme);
	/* If keyword, return Keyword token. */
	if (kw_index != -1) {
		t.code = KW_T;
		t.attribute.kwt_idx = kw_index;
		return t;
	}
	t.code = AVID_T;
	/* Check length of non-keyword character array, check length against max allowed. If too long
	trim the lexeme. Else copy full lexeme. */
	if (strlen(lexeme) > VID_LEN) {
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else strcpy(t.attribute.vid_lex, lexeme);
	/* Return Variable token. */
	return t;
}
/*
* Purpose: Take the input character array and parse it to produce a valid string identifier or error token.
* Author: James Holmes
* Version: 1.01
* Called Functons: strcpy(), strlen(), strncpy()
* Parameters: lexeme-> the charcter array used to produce the return token.
* Return: A string identifier token on success, else return an error token.
*/
Token aa_func03(char lexeme[]) {
	Token t; /* The new SVID Token to return. */
			 /* Check for NULL lexeme */
	/*if (!lexeme) { scerrnum = 1; strcpy(t.attribute.err_lex, "RUN TIME ERROR"); t.code = ERR_T; return t; }*/
	/* Check length of passed lexeme against max allowed length. Trim if longer. */
	if (strlen(lexeme) > VID_LEN) {
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN - 1);
		t.attribute.vid_lex[VID_LEN - 1] = '$';
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else strcpy(t.attribute.vid_lex, lexeme);
	t.code = SVID_T;
	return t;
}
/*
* Purpose: Takes a character array to parse and produce a Float or Error token.
* Author: James Holmes
* Version: 1.01
* Called Functons: strcpy(), strcat(), strncpy(), atof()
* Parameters: lexeme-> character array used to produce the token.
* Returns: A Floating point literal token on success, Error token on failure.
*/
Token aa_func08(char lexeme[]) {
	Token t; /* Token to return. */
	double lextofloat; /* Used to store converted lexeme value. */
					   /* Check for NULL lexeme */
	/*if (!lexeme) { scerrnum = 1; strcpy(t.attribute.err_lex, "RUN TIME ERROR"); t.code = ERR_T; return t; }*/
	/* Convert the lexeme to a double */
	lextofloat = atof(lexeme);
	/* Verify the value of the converted lexeme against its type limit. Return Float token if valid.*/
	if ((lextofloat > FLT_MIN && lextofloat < FLT_MAX) || lextofloat == 0.0) {
		t.code = FPL_T;
		t.attribute.flt_value = (float)lextofloat;
		return t;
	}
	t.code = ERR_T;
	/* If the converted value is not valid return an Error token. If the lexeme is more characters than the
	max allowed, trim it to max allowed minus three, add three "." to its tail to match max length.*/
	if (strlen(lexeme) > ERR_LEN) {
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		t.attribute.vid_lex[ERR_LEN - 3] = '\0';
		strcat(t.attribute.err_lex, "...");
	}
	else strcpy(t.attribute.err_lex, lexeme);
	return t;
}
/*
* Purpose: Takes a passed character array and parses it to attempt to produce an Integer token.
* Author: James Holmes
* Version: 1.01
* Called Functons:strcpy(), atol(), strlen(), strncpy(), strcat(), strcpy()
* Parameters: lexeme-> the character array to attempt to parse into an Integer token.
* Returns: An Integer token on success, else Error token.
*/
Token aa_func05(char lexeme[]) {
	Token t; /* New Token to return. */
	long lextoint; /* Used to store the converted lexeme value.*/
					 /* Check for a NULL lexeme. */
	/*if (!lexeme) { scerrnum = 1; strcpy(t.attribute.err_lex, "RUN TIME ERROR"); t.code = ERR_T; return t; }*/
	/* Convert the lexeme to an integer. */
	lextoint = atol(lexeme);
	/* Check the convert value against type limits. If valid return an Integer token. */
	if (lextoint <= _I16_MAX) {
		t.code = INL_T;
		t.attribute.int_value = (int)lextoint;
		return t;
	}
	t.code = ERR_T;
	/* If  invalid integer, produce and return an Error token. If the attribute is too long,
	trim it to max allowed minus three. */
	if (strlen(lexeme) > ERR_LEN) {
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		t.attribute.vid_lex[ERR_LEN - 3] = '\0';
		strcat(t.attribute.err_lex, "...");
	}
	else strcpy(t.attribute.err_lex, lexeme);
	return t;
}
/*
* Purpose: Take a passed character array and parse it to attempt to produce a Hex token.
* Author: Adam White
* Version: 1.01
* Called Functons: atolh(), strlen(), strncpy(), strcat(), strcpy()
* Parameters:
* Returns: A Hex token on success, else Error token.
*/
Token aa_func11(char lexeme[]) {
	Token t; /* Token to return. */
	long lextoint; /* Used to store converted lexeme value. */
				   /* Convert the lexeme to hex. */
	lextoint = atolh(lexeme);
	/* Check the converted value against its type limits. If valid produce and return a Hex token. */
	if (lextoint <= _I16_MAX) {
		t.code = INL_T;
		t.attribute.int_value = (int)lextoint;
		return t;
	}
	/* If coverted value is invalid produce and return an Error token. If the attribute exceeds the max
	allowed length trim it.*/
	t.code = ERR_T;
	if (strlen(lexeme) > ERR_LEN) {
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		t.attribute.vid_lex[ERR_LEN - 3] = '\0';
		strcat(t.attribute.err_lex, "...");
	}
	else strcpy(t.attribute.err_lex, lexeme);
	return t;
}
/*
* Purpose: Take in passed lexeme and produces a Error token with it.
* Author: Adam White
* Version: 1.01
* Called Functons: strlen(). strnpy(), strcat(), strcpy()
* Parameters: lexeme-> character array used to produce the token.
* Return: An Error token.
*/
Token aa_func12(char lexeme[]) {
	Token t; /* Token to return. */
			 /* Set Error token, if the lexeme exceeds the max allowed length trim it. */
	t.code = ERR_T;
	if (strlen(lexeme) > ERR_LEN) {
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		t.attribute.vid_lex[ERR_LEN] = '\0';
		strcat(t.attribute.err_lex, "...");
	}
	else strcpy(t.attribute.err_lex, lexeme);
	return t;
}
/*
* Purpose: Convert a character array to its hex equivalent value.
* Author: Adam White
* Version: 1.01
* Called Functons: strlen()
* Parameters: lexeme-> The character array to convert to hex values.
* Returns: The hex equivalent of the passed character array.
*/
long atolh(char * lexeme) {
	long val = 0; /* Value to return. */
	long base = 1; /* Base to multiply by character position. */
	long index;
	lexeme++; /* Skip the "0x" of the lexeme. */
	lexeme++;
	index = strlen(lexeme) - 1; /* Character position tracker. */
								/* Convert each single position to its hex value based on its position in the string. */
	for (; index >= 0; index--) {
		switch (lexeme[index]) {
		case 'A': val += base * 10; base *= 16; break;
		case 'B': val += base * 11; base *= 16; break;
		case 'C': val += base * 12; base *= 16; break;
		case 'D': val += base * 13; base *= 16; break;
		case 'E': val += base * 14; base *= 16; break;
		case 'F': val += base * 15; base *= 16; break;
		default: val += base*(lexeme[index] - '0'); base *= 16; break;
		}
	}
	/* Return the coverted hex value. */
	return val;
}
/*
* Purpose: Check the passed character array against all Keywords.
* Author: Adam White
* Version: 1.01
* Called Functons: strcmp()
* Parameters: kw_lexeme-> character pointer to check against all kewords.
* Returns: Keyword index if matched, -1 if non-match.
*/
char keyword_index(char * kw_lexeme) {
	char i = 0;
	for (i; i < KWT_SIZE; i++) {
		if (!strcmp(kw_lexeme, kw_table[i]))
			return i;
	}
	return -1;
}