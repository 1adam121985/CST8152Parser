/*
* File Name: parser.c
* Compiler: MS Visual Studio 2015
* Author: Adam White 040 518 422 James Holmes 040 776 963
* Course: CST 8152 - Compilers, Lab Section: 11/13 (respectively)
* Assignment: 3
* Date: January 5, 2018
* Professor: Sv. Ranev
* Purpose: Parser utilites functions
* Function list:
* parser()
* match()
* syn_eh()
* printe()
* gen_incode()
* grammar production functions
*/

/* project header files */
#include <stdlib.h>
#include "parser.h"


static Token lookahead;/*next token to be parsed*/
static Buffer* sc_buf;/*ponter to input buffer*/
int synerrno;/*number of errors*/
extern Token malar_next_token(Buffer * sc_buf);/*defined in scanner.c, returns next token*/
extern char * kw_table[];/*defined in table.h, string array of keywords*/
extern Buffer * str_LTBL;/*defined in platy.c, pointer to string literal table*/
extern int line;/*defined in platy.c, line number*/

/*
* Purpose: Parses input buffer ponted to by in_buf
* Author: Sv Ranev
* Version: 1
* Called Functons: malar_next_token(), program(), match(),gen_incode()
* Parameters: in_buf-> pointer to input buffer
* Returns: void
*/
void parser(Buffer * in_buf) {
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTRIBUTE);
	gen_incode("PLATY: Source file parsed");
}
/*
* Purpose: Checks that next token is valid
* Author: Adam White, James Holmes
* Version: 1.0
* Called Functons: malar_next_token(), syn_printe(), match(),gen_incode(), syn_eh()
* Parameters: pr_token_code-> token code to mach
*			  pr_token_attribute-> token attribute to match
* Returns: void
* Algorithm: Checks that next token matches that suggested by its parameters and calls appropriate grammar prodution functions
* handles non-matches by printing error
*/
void match(int pr_token_code, int pr_token_attribute) {
	/*if (pr_token_code == lookahead.code == SEOF_T) return;*/
	if (pr_token_code == SEOF_T && lookahead.code == SEOF_T) return;
	/*if (pr_token_code == ERR_T == lookahead.code) {*/
	if (pr_token_code == ERR_T && lookahead.code == ERR_T) {
		syn_printe();
		malar_next_token(sc_buf);
		++synerrno;
		return;
	}
	if (lookahead.code == pr_token_code) {
		if (pr_token_code != KW_T && pr_token_code != LOG_OP_T && pr_token_code != ART_OP_T && pr_token_code != REL_OP_T) {
			lookahead = malar_next_token(sc_buf);
		}
		else if ((pr_token_code == KW_T && pr_token_attribute == lookahead.attribute.kwt_idx)
			|| (pr_token_code == LOG_OP_T && pr_token_attribute == lookahead.attribute.log_op)
			|| (pr_token_code == ART_OP_T && pr_token_attribute == lookahead.attribute.arr_op)
			|| (pr_token_code == REL_OP_T && pr_token_attribute == lookahead.attribute.rel_op)) {
			lookahead = malar_next_token(sc_buf);
		}
		else { syn_eh(pr_token_code); return; }
		if (lookahead.code == ERR_T) { syn_printe(); lookahead = malar_next_token(sc_buf); ++synerrno; return; }

		/*if ((pr_token_code == KW_T && pr_token_attribute == lookahead.attribute.kwt_idx)
			|| (pr_token_code == LOG_OP_T && pr_token_attribute == lookahead.attribute.log_op)
			|| (pr_token_code == ART_OP_T && pr_token_attribute == lookahead.attribute.arr_op)
			|| (pr_token_code == REL_OP_T && pr_token_attribute == lookahead.attribute.rel_op)) lookahead = malar_next_token(sc_buf);*/
			/*else { syn_eh(pr_token_code); return; }*/


	}
	else { syn_eh(pr_token_code); return; }
}
/*
* Purpose: Checks that next token is valid
* Author: Adam White, James Holmes
* Version: 1.0
* Called Functons: malar_next_token(), syn_printe(), match(),gen_incode(), syn_eh()
* Parameters: pr_token_code-> token code to mach
*			  pr_token_attribute-> token attribute to match
* Returns: void
* Algorithm: Checks that next token matches that suggested by its parameters and calls appropriate grammar prodution functions
* handles non-matches by printing error
*/
void syn_eh(int sync_token_code) {
	syn_printe();
	++synerrno;
	while (1) {
		lookahead = malar_next_token(sc_buf);
		if (lookahead.code != sync_token_code && lookahead.code == SEOF_T) exit(synerrno);
		if (lookahead.code == sync_token_code && lookahead.code == SEOF_T) return;
		if (lookahead.code == sync_token_code) { lookahead = malar_next_token(sc_buf); return; }

	}
}
/*
* Purpose: Prints coorsponding error based on lookahead tokens code
* Author: Adam White, James Holmes
* Version: 1.0
* Called Functons: printf(),
* Parameters: void
* Returns: void
* Algorithm: 
*/
void printe(void) {
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
		printf("PLATY: Syntax error:  Line: %d\n*****  Token code: %d Attribute: %s", line, lookahead.code, lookahead.attribute.vid_lex);
	else if (lookahead.code == STR_T)
		printf("PLATY: Syntax error:  Line: %d\n*****  Token code: %d Attribute: %s", line, lookahead.code, b_location(str_LTBL, lookahead.attribute.str_offset));
	else if (lookahead.code == KW_T)
		printf("PLATY: Syntax error:  Line: %d\n*****  Token code: %d Attribute: %s", line, lookahead.code, kw_table[lookahead.attribute.kwt_idx]);
	else if (lookahead.code == INL_T)
		printf("PLATY: Syntax error:  Line: %d\n*****  Token code: %d Attribute: %d", line, lookahead.code, lookahead.attribute.int_value);
	else if (lookahead.code == FPL_T)
		printf("PLATY: Syntax error:  Line: %d\n*****  Token code: %d Attribute: %f", line, lookahead.code, lookahead.attribute.flt_value);
	else if (lookahead.code == ERR_T)
		printf("PLATY: Syntax error:  Line: %d\n*****  Token code: %d Attribute: %s", line, lookahead.code, lookahead.attribute.err_lex);
	else
		printf("PLATY: Syntax error:  Line: %d\n*****  Token code: %d Attribute: NA", line, lookahead.code);
}
/*
* Purpose: Prints based character*
* Author: Adam White, James Holmes
* Version: 1.0
* Called Functons: malar_next_token(), syn_printe(), match(),gen_incode(), syn_eh()
* Parameters: to_print -> character* to print
* Returns: void
* Algorithm: 
*/
void gen_incode(char * to_print) {
	printf("%s\n", to_print);
}

/*
<program> -> PLATYPUS {<opt_statements>}
First(<program>) = {PLATYPUS}
*/
void program(void) {
#ifdef DEBUG
	printf("program\n");
#endif // DEBUG

	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTRIBUTE); opt_statements(); match(RBR_T, NO_ATTRIBUTE);
	gen_incode("PLATY: Program parsed");
}

/*
<opt_statements> -> <statements> | ɛ
First(<opt_statements>) = {AVID_T, SVID_T, IF, WHILE, READ, WRITE, ɛ}
*/
void opt_statements(void) {
#ifdef DEBUG
	printf("opt_statements\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		if (lookahead.attribute.kwt_idx == IF
			|| lookahead.attribute.kwt_idx == WHILE
			|| lookahead.attribute.kwt_idx == READ
			|| lookahead.attribute.kwt_idx == WRITE) {
			statements(); break;
		}
	default:
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*
<statements> -> <statement><statements'>
First(<statements>) = {AVID_T, SVID_T, IF, WHILE, READ, WRITE}
*/
void statements(void) {
#ifdef DEBUG
	printf("statements\n");
#endif // DEBUG
	statement(); statements_prime();
}

/*<statements'> -> <statement><statements'> | ɛ
First(<statements'>) = {AVID_T, SVID_T, IF, WHILE, READ, WRITE, ɛ}
*/
void statements_prime(void) {
#ifdef DEBUG
	printf("statements'\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statement(); statements_prime(); break;
	case KW_T:
		if (lookahead.attribute.kwt_idx == IF
			|| lookahead.attribute.kwt_idx == WHILE
			|| lookahead.attribute.kwt_idx == READ
			|| lookahead.attribute.kwt_idx == WRITE) {
			statement(); statements_prime(); break;
		}
		/*default:*/
			/*gen_incode("PLATY: Statements_prime parsed");*/
	}
}

/*
<statement> -> <assignment statement> | <selection statement> | <iteration statement> | <input statement> | <output statement>
First(<statement>) = {AVID_T, SVID_T, IF, WHILE, READ, WRITE}
*/
void statement(void) {
#ifdef DEBUG
	printf("statement\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: assignment_statement(); break;
	case KW_T:
		if (lookahead.attribute.kwt_idx == IF) { selection_statement(); break; }
		if (lookahead.attribute.kwt_idx == WHILE) { iteration_statement(); break; }
		if (lookahead.attribute.kwt_idx == READ) { input_statement(); break; }
		if (lookahead.attribute.kwt_idx == WRITE) { output_statement(); break; }
	default:
		syn_printe();
	}
}

/*
<assignment statement> -> <assignment expression>;
First(<assignment statement>) = {AVID_T, SVID_T}
*/
void assignment_statement(void) {
#ifdef DEBUG
	printf("assignment_statement\n");
#endif // DEBUG
	assignment_expression();
	match(EOS_T, NO_ATTRIBUTE);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
<assignment expression> -> AVID_T = <arithmetic expression> | SVID_T = <string expression>
First(<assignment expression>) = {AVID_T, SVID_T}
*/
void assignment_expression(void) {
#ifdef DEBUG
	printf("assignment_expression\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTRIBUTE); match(ASS_OP_T, NO_ATTRIBUTE); arithmetic_expression(); gen_incode("PLATY: Assignment expression (arithmetic) parsed"); break;
	case SVID_T: match(SVID_T, NO_ATTRIBUTE); match(ASS_OP_T, NO_ATTRIBUTE); string_expression(); gen_incode("PLATY: Assignment expression (string) parsed"); break;
	default: syn_printe(); gen_incode("PLATY: Assignment expression (arithmetic) parsed");
	}
}

/*
<selection statement> -> IF TRUE (<conditional expression>) THEN {<opt_statements>} ELSE {opt_statements>};
First(<selection statement>) = {IF}
*/
void selection_statement(void) {
#ifdef DEBUG
	printf("selection_statement\n");
#endif // DEBUG
	match(KW_T, IF); match(KW_T, TRUE); match(LPR_T, NO_ATTRIBUTE); conditional_expression();
	match(RPR_T, NO_ATTRIBUTE); match(KW_T, THEN); match(LBR_T, NO_ATTRIBUTE); opt_statements();
	match(RBR_T, NO_ATTRIBUTE); match(KW_T, ELSE); match(LBR_T, NO_ATTRIBUTE); opt_statements();
	match(RBR_T, NO_ATTRIBUTE); match(EOS_T, NO_ATTRIBUTE); gen_incode("PLATY: Selection statement parsed");
}

/*
<iteration statement> -> WHILE <pre-condition>(<conditional expression>) REPEAT {<statements>};
First(<iteration statement>) = {WHILE}
*/
void iteration_statement(void) {
#ifdef DEBUG
	printf("iteration_statement\n");
#endif // DEBUG
	match(KW_T, WHILE); pre_condition(); match(LPR_T, NO_ATTRIBUTE); conditional_expression();
	match(RPR_T, NO_ATTRIBUTE); match(KW_T, REPEAT); match(LBR_T, NO_ATTRIBUTE); statements();
	match(RBR_T, NO_ATTRIBUTE); match(EOS_T, NO_ATTRIBUTE); gen_incode("PLATY: Iteration statement parsed");
}

/*
<pre-condition> -> TRUE | FALSE
First(<pre-condition>) = {TRUE, FALSE}
*/
void pre_condition(void) {
#ifdef DEBUG
	printf("pre_condition\n");
#endif // DEBUG
	switch (lookahead.code) {
	case KW_T:
		if (lookahead.attribute.kwt_idx == TRUE) { match(KW_T, TRUE); break; }
		if (lookahead.attribute.kwt_idx == FALSE) { match(KW_T, FALSE); break; }
	default:
		syn_printe();
	}
}

/*
<input statement> -> READ (<variable list>);
First(<input statement>) = {READ}
*/
void input_statement(void) {
#ifdef DEBUG
	printf("input_statement\n");
#endif // DEBUG
	match(KW_T, READ); match(LPR_T, NO_ATTRIBUTE); variable_list();
	match(RPR_T, NO_ATTRIBUTE); match(EOS_T, NO_ATTRIBUTE); gen_incode("PLATY: Input statement parsed");
}
/*
<variable list> -> <variable identifier><variable list'>
	First(<variable list>) = { SVID_T, AVID_T }
*/
void variable_list(void) {
#ifdef DEBUG
	printf("variable_list\n");
#endif // DEBUG
	variable_identifier(); variable_list_prime(); gen_incode("PLATY: Variable list parsed");
}

/*
<variable list'> -> ,<variable identifier><variable list'> | ɛ
First(<variable list'>) = {SVID_T, AVID_T, ɛ}
*/
void variable_list_prime(void) {
#ifdef DEBUG
	printf("variable_list'\n");
#endif // DEBUG
	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTRIBUTE);
		variable_identifier();
		variable_list_prime();
		break;
		/*default:*/
			/*gen_incode("PLATY: variable_list_prime parsed");*/
	}
}

/*
<variable identifier> -> AVID_T | SVID_T
First(<variable identifier>) = {SVID_T, AVID_T}
*/
void variable_identifier(void) {
#ifdef DEBUG
	printf("variable_identifier\n");
#endif // DEBUG
	switch (lookahead.code) {
	case SVID_T: match(SVID_T, NO_ATTRIBUTE); break;
	case AVID_T: match(AVID_T, NO_ATTRIBUTE); break;
	default: syn_printe();
	}
}

/*
<output statement> -> WRITE(<output list>);
First(<output statement>) = {WRITE}
*/
void output_statement(void) {
#ifdef DEBUG
	printf("output_statement\n");
#endif // DEBUG
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTRIBUTE);
	output_list();
	match(RPR_T, NO_ATTRIBUTE);
	match(EOS_T, NO_ATTRIBUTE);
	gen_incode("PLATY: Output statement parsed");
}

/*
<output list> -> <opt_variable list> | STR_T
First(<output list>) = {SVID_T, AVID_T, STR_T, ɛ}
*/
void output_list(void) {
#ifdef DEBUG
	printf("output_list\n");
#endif // DEBUG
	switch (lookahead.code) {
	case STR_T:
		match(STR_T, NO_ATTRIBUTE); gen_incode("PLATY: Output list (string literal) parsed"); break;
	case AVID_T: case SVID_T: opt_variable_list(); break;
	default: opt_variable_list(); gen_incode("PLATY: Output list (empty) parsed");
	}
}

/*
<opt_variable list> -> <variable list> | ɛ
First(<opt_variable list>) = {SVID_T, AVID_T, ɛ}
*/
void opt_variable_list(void) {
#ifdef DEBUG
	printf("opt_variable_list\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variable_list(); break;
		/*default:*/
			/*gen_incode("PLATY: optional variable list parsed.");*/
	}
}

/*
<arithmetic expression> -> <unary arithmetic expression> | <additive arithmetic expression>
First(<arithmetic expression>) = {+,-,AVID_T, FPL_T, INL_T, (}
*/
void arithmetic_expression(void) {
#ifdef DEBUG
	printf("arithmetic_expression\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T: additive_arithmetic_expression(); gen_incode("PLATY: Arithmetic expression parsed"); break;
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS
			|| lookahead.attribute.arr_op == MINUS) {
			unary_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		}
	default:
		syn_printe(); gen_incode("PLATY: Arithmetic expression parsed");
	}
}

/*
<unary arithmetic expression> -> <unary operator><primary arithmetic expression>
First(<unary arithmetic expression>) = {+,-}
*/
void unary_arithmetic_expression(void) {
#ifdef DEBUG
	printf("unary_arithmetic_expression\n");
#endif // DEBUG
	unary_opertor(); primary_arithmetic_expression(); gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*
<unary operator> -> + | -
First(<unary operator>) = {+,-}
*/
void unary_opertor(void) {
#ifdef DEBUG
	printf("unary_opertor\n");
#endif // DEBUG
	switch (lookahead.code)
	{
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS) { match(ART_OP_T, PLUS); break; }
		if (lookahead.attribute.arr_op == MINUS) { match(ART_OP_T, MINUS); break; }
	default:
		syn_printe();
	}
}

/*
<additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression'>
First(<additive arithmetic expression>) = {AVID_T, FPL_T, INL_T, (}
*/
void additive_arithmetic_expression(void) {
#ifdef DEBUG
	printf("additive_arithmetic_expression\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T:case FPL_T: case INL_T: case LPR_T:
		multiplicative_arithmetic_expression();
		if (lookahead.code == ART_OP_T && (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS))
		{
			additive_arithmetic_expression_prime(); gen_incode("PLATY: Additive arithmetic expression parsed");
		}
		break;
	default: syn_printe(); gen_incode("PLATY: Additive arithmetic expression parsed");
	}
}

/*
<additive arithmetic expression'> -> <additive arithmetic expression operator><multiplicative arithmetic expression><additive arithmetic expression'> | ɛ
First(<additive arithmetic expression'>) = {+, -, ɛ}
*/
void additive_arithmetic_expression_prime(void) {
#ifdef DEBUG
	printf("additive_arithmetic_expression'\n");
#endif // DEBUG
	switch (lookahead.code) {
	case ART_OP_T:
		/*if (lookahead.attribute.arr_op != PLUS || lookahead.attribute.arr_op != MINUS) break;
		if (lookahead.attribute.arr_op == PLUS)	match(ART_OP_T, PLUS);
		else match(ART_OP_T, MINUS);*/
		additive_arithmetic_expression_operator(); multiplicative_arithmetic_expression();
		additive_arithmetic_expression_prime(); break;
		/*if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS) { additive_arithmetic_expression_operator(); multiplicative_arithmetic_expression(); additive_arithmetic_expression_prime(); break; }*/
	/*default:*/
		/*gen_incode("PLATY: additive_arithmetic_expression_prime parsed");*/
	}
}

/*
<additive arithmetic expression operator> -> + | -
First(<additive arithmetic expression operator>) = {+, -}
*/
void additive_arithmetic_expression_operator(void) {
#ifdef DEBUG
	printf("additive_arithmetic_expression_operator\n");
#endif // DEBUG
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS) { match(ART_OP_T, PLUS); break; }
		if (lookahead.attribute.arr_op == MINUS) { match(ART_OP_T, MINUS); break; }
	default: syn_printe();
	}
}

/*
<multiplicative arithmetic expression> -> <primary arithmetic expression><multiplicative arithmetic expression'>
First(<multiplicative arithmetic expression>) = {AVID_T, FPL_T, INL_T, (}
*/
void multiplicative_arithmetic_expression(void) {
#ifdef DEBUG
	printf("multiplicative_arithmetic_expression\n");
#endif // DEBUG
	primary_arithmetic_expression();
	if (lookahead.code == ART_OP_T && (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV)) {
		multiplicative_arithmetic_expression_prime();
		gen_incode("PLATY: Multiplicative arithmetic expression parsed");
	}
}

/*
<multiplicative arithmetic expression'> -> <multiplicative arithmetic expression operator><primary arithmetic expression><multiplicative arithmetic expression'> | ɛ
First(<multiplicative arithmetic expression'>) = {*, /, ɛ}
*/
void multiplicative_arithmetic_expression_prime(void) {
#ifdef DEBUG
	printf("multiplicative_arithmetic_expression'\n");
#endif // DEBUG
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV) { multiplicative_arithmetic_expression_operator(); primary_arithmetic_expression(); multiplicative_arithmetic_expression_prime(); break; }
		/*default:*/
			/*gen_incode("PLATY: multiplicative_arithmetic_expression_prime parsed");*/
	}
}

/*
<multiplicative arithmetic expression operator> -> * | /
First(<multiplicative arithmetic expression operator>) = {*, /}
*/
void multiplicative_arithmetic_expression_operator(void) {
#ifdef DEBUG
	printf("multiplicative_arithmetic_expression_operator\n");
#endif // DEBUG
	switch (lookahead.code)
	{
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT) { match(ART_OP_T, MULT); break; }
		if (lookahead.attribute.arr_op == DIV) { match(ART_OP_T, DIV); break; }
	default:
		syn_printe();
	}
}

/*
<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
First(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, (}
*/
void primary_arithmetic_expression(void) {
#ifdef DEBUG
	printf("primary_arithmetic_expression\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary arithmetic expression parsed"); break;
	case FPL_T: match(FPL_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary arithmetic expression parsed"); break;
	case INL_T: match(INL_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary arithmetic expression parsed"); break;
	case LPR_T: match(LPR_T, NO_ATTRIBUTE); arithmetic_expression(); match(RPR_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary arithmetic expression parsed"); break;
	default: syn_printe(); gen_incode("PLATY: Primary arithmetic expression parsed");
	}
}

/*
<string expression> -> <primary string expression><string expression'>
First(<string expression>) = {SVID_T, STR_T}
*/
void string_expression(void) {
#ifdef DEBUG
	printf("string_expression\n");
#endif // DEBUG
	primary_string_expression(); string_expression_prime(); gen_incode("PLATY: String expression parsed");
}

/*
<string expression'> -> #<primary string expression><string expression'> | ɛ
First(<string expression'>) = {#, ɛ}
*/
void string_expression_prime(void) {
#ifdef DEBUG
	printf("string_expression'\n");
#endif // DEBUG
	switch (lookahead.code)
	{
	case SCC_OP_T: match(SCC_OP_T, NO_ATTRIBUTE); primary_string_expression(); string_expression_prime(); break;
		/*default:*/
			/*gen_incode("PLATY: string_expression_prime parsed");*/
	}
}

/*
<primary string expression> -> SVID_T | STR_T
First(<primary string expression>) = {SVID_T, STR_T}
*/
void primary_string_expression(void) {
#ifdef DEBUG
	printf("primary_string_expression\n");
#endif // DEBUG
	switch (lookahead.code)
	{
	case SVID_T: match(SVID_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary string expression parsed"); break;
	case STR_T: match(STR_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary string expression parsed"); break;
	default: syn_printe();
	}
}

/*
<conditional expression> -> <logical OR expression>
First(<conditional expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void conditional_expression(void) {
#ifdef DEBUG
	printf("conditional_expression\n");
#endif // DEBUG
	logical_or_expression(); gen_incode("PLATY: Conditional expression parsed");
}

/*
<logical OR expression> -> <logical AND expression><logical OR expression'>
First(<logical OR expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_or_expression(void) {
#ifdef DEBUG
	printf("logical_or_expression\n");
#endif // DEBUG
	logical_and_expression();
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == OR) {
		logical_or_expression_prime();
		gen_incode("PLATY: Logical OR expression parsed");
	}
}

/*
<logical OR expression'> -> .OR.<logical AND expression><logical OR expression'> | ɛ
First(<logical OR expression'>) = {.OR., ɛ}
*/
void logical_or_expression_prime(void) {
#ifdef DEBUG
	printf("logical_or_expression'\n");
#endif // DEBUG
	switch (lookahead.code) {
	case LOG_OP_T:
		if (lookahead.attribute.log_op == OR)match(LOG_OP_T, OR); logical_and_expression(); logical_or_expression_prime(); break;
		/*default:*/
			/*gen_incode("PLATY: logical_or_expression_prime parsed");*/
	}
}

/*
<logical AND expression> -> <relational expression><logical AND expression'>
First(<logical AND expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_and_expression(void) {
#ifdef DEBUG
	printf("logical_and_expression\n");
#endif // DEBUG
	relational_expression();
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == AND)
	{
		logical_and_expression_prime(); gen_incode("PLATY: Logical AND expression parsed");
	}
}

/*
<logical AND expression'> -> .AND.<relational expression><logical AND expression'> | ɛ
First(<logical AND expression'>) = {.AND., ɛ}
*/
void logical_and_expression_prime(void) {
#ifdef DEBUG
	printf("logical_and_expression'\n");
#endif // DEBUG
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == AND) {
		match(LOG_OP_T, AND); relational_expression(); logical_and_expression_prime();
	}
	/*default:*/
		/*gen_incode("PLATY: logical_and_expression_prime parsed");*/
}

/*
<relational expression> -> <primary a_relational expression><relational operator><primary a_relational expression>
|  <primary s_relational expression><relational operator><primary s_relational expression>
First(<relational expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void relational_expression(void) {
#ifdef DEBUG
	printf("relational_expression\n");
#endif // DEBUG
	switch (lookahead.code) {
	case SVID_T: case STR_T:
		primary_s_relational_expression(); relational_operator(); primary_s_relational_expression(); gen_incode("PLATY: Relational expression parsed"); break;
	case AVID_T:case FPL_T:case INL_T:
		primary_a_relational_expression(); relational_operator(); primary_a_relational_expression(); gen_incode("PLATY: Relational expression parsed"); break;
	default: syn_printe(); gen_incode("PLATY: Relational expression parsed");
	}
}

/*
<relational operator> -> == | <> | < | >
First(<relational operator>) = {==, <>, <, >}
*/
void relational_operator(void) {
#ifdef DEBUG
	printf("relational_operator\n");
#endif // DEBUG
	switch (lookahead.code)
	{
	case REL_OP_T:
		if (lookahead.attribute.rel_op == EQ) { match(REL_OP_T, EQ); break; }
		if (lookahead.attribute.rel_op == NE) { match(REL_OP_T, NE); break; }
		if (lookahead.attribute.rel_op == GT) { match(REL_OP_T, GT); break; }
		if (lookahead.attribute.rel_op == LT) { match(REL_OP_T, LT); break; }
	default:
		syn_printe();
	}
}

/*
<primary a_relational expression> -> AVID_T | FPL_T | INL_T
First(<primary a_relational expression>) = {AVID_T, FPL_T, INL_T}
*/
void primary_a_relational_expression(void) {
#ifdef DEBUG
	printf("primary_a_relational_expression\n");
#endif // DEBUG
	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary a_relational expression parsed"); break;
	case FPL_T: match(FPL_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary a_relational expression parsed"); break;
	case INL_T: match(INL_T, NO_ATTRIBUTE); gen_incode("PLATY: Primary a_relational expression parsed"); break;
	default: syn_printe(); gen_incode("PLATY: Primary a_relational expression parsed");
	}
}

/*
<primary s_relational expression> -> <primary string expression>
First(<primary s_relational expression>) = {SVID_T, STR_T}
*/
void primary_s_relational_expression(void) {
#ifdef DEBUG
	printf("primary_s_relational_expression\n");
#endif // DEBUG
	switch (lookahead.code) {
	case SVID_T: case STR_T:
		primary_string_expression(); gen_incode("PLATY: Primary s_relational expression parsed"); break;
	default: syn_printe(); gen_incode("PLATY: Primary s_relational expression parsed");
	}
}

/* error printing function for Assignment 3 (Parser), F17 */
void syn_printe(void) {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/