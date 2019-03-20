/*
* File Name: table.h
* Compiler: MS Visual Studio 2015
* Author: Adam White 040 518 422 James Holmes 040 776 963
* Course: CST 8152 - Compilers, Lab Section: 11/13 (respectively)
* Assignment: 3
* Date: January 5, 2018
* Professor: Sv. Ranev
* Purpose: Preprocessor directives for parser, defines for index of keywords, function prototypes
*/
#ifndef PARSER_H_
#define PARSER_H_

#include "buffer.h"
#include "token.h"

#define NO_ATTRIBUTE -2/*No token attribute*/
#define ELSE 0/*Keyword index position for keyword ELSE*/
#define FALSE 1/*Keyword index position for keyword FALSE*/
#define IF 2/*Keyword index position for keyword IF*/
#define PLATYPUS 3/*Keyword index position for keyword PLATYPUS*/
#define READ 4/*Keyword index position for keyword READ*/
#define REPEAT 5/*Keyword index position for keyword REPEAT*/
#define THEN 6/*Keyword index position for keyword THEN*/
#define TRUE 7/*Keyword index position for keyword TRUE*/
#define WHILE 8/*Keyword index position for keyword WHILE*/
#define WRITE 9/*Keyword index position for keyword WRITE*/

/*Function Definitions*/
void parser(Buffer * in_buf);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);/*panic mode error handler*/
void printe();/*error printer*/
void gen_incode(char * to_print);/*intented to generate code, but prints message indicating a non-terminal parsed*/
void program(void);/*parses production <program>*/
void opt_statements();/*parses production <opt_statements>*/
void statements(void);/*parses production <statements>*/
void statements_prime();/*parses production <statements_prime>*/
void statement();/*parses production <statement>*/
void assignment_statement(void);/*parses production <assignment_statement>*/
void assignment_expression(void);/*parses production <assignment_expression>*/
void selection_statement(void);/*parses production <selection_statement>*/
void iteration_statement(void);/*parses production <iteration_statement>*/
void pre_condition();/*parses production <pre_condition>*/
void input_statement(void);/*parses production <input_statement>*/
void variable_list(void);/*parses production <variable_list>*/
void variable_list_prime();/*parses production <variable_list_prime>*/
void variable_identifier();/*parses production <variable_identifier>*/
void output_statement(void);/*parses production <output_statement>*/
void output_list(void);/*parses production <output_list>*/
void opt_variable_list(void);/*parses production <opt_variable_list>*/
void arithmetic_expression(void);/*parses production <arithmetic_expression>*/
void unary_arithmetic_expression(void);/*parses production <unary_arithmetic_expression>*/
void unary_opertor();/*parses production <unary_opertor>*/
void additive_arithmetic_expression();/*parses production <additive_arithmetic_expression>*/
void additive_arithmetic_expression_prime();/*parses production <additive_arithmetic_expression_prime>*/
void additive_arithmetic_expression_operator();/*parses production <additive_arithmetic_expression_operator>*/
void multiplicative_arithmetic_expression(void);/*parses production <multiplicative_arithmetic_expression>*/
void multiplicative_arithmetic_expression_prime();/*parses production <multiplicative_arithmetic_expression_prime>*/
void multiplicative_arithmetic_expression_operator();/*parses production <multiplicative_arithmetic_expression_operator>*/
void primary_arithmetic_expression();/*parses production <primary_arithmetic_expression>*/
void string_expression(void);/*parses production <string_expression>*/
void string_expression_prime();/*parses production <string_expression_prime>*/
void primary_string_expression();/*parses production <primary_string_expression>*/
void conditional_expression(void);/*parses production <conditional_expression>*/
void logical_or_expression(void);/*parses production <logical_or_expression>*/
void logical_or_expression_prime();/*parses production <logical_or_expression_prime>*/
void logical_and_expression(void);/*parses production <logical_and_expression>*/
void logical_and_expression_prime();/*parses production <logical_and_expression_prime>*/
void relational_expression();/*parses production <relational_expression>*/
void relational_operator();/*parses production <relational_operator>*/
void primary_a_relational_expression();/*parses production <primary_a_relational_expression>*/
void primary_s_relational_expression();/*parses production <primary_s_relational_expression>*/
void syn_printe();/*another error printing function*/
#endif
