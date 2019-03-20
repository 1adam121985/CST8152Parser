/*
* File Name: table.h
* Compiler: MS Visual Studio 2015
* Author: Adam White 040 518 422 James Holmes 040 776 963
* Course: CST 8152 - Compilers, Lab Section: 11/13 (respectively)
* Assignment: 2
* Date: November 30, 2017
* Professor: Sv. Ranev
* Purpose: Preprocessor directives for acceptng function prototypes and array of their pointers, transition table, keywords, accepting states/nonaccepting states
* Function list:
*	aa_func02()
*	aa_func11()
*	aa_func03()
*	aa_func05()
*	aa_func08()
*	aa_func11()
*	aa_func12()
*/

#define _CRT_SECURE_NO_WARNINGS
#include "buffer.h"

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or only one of the folowing constants: 255, 0xFF , EOF
*/

/*  Single-lexeme tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
*       space
*  !!comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', '#' ,
*  .AND., .OR. , SEOF, 'wrong symbol',
*/


#define ES  12 /* Error state */
#define ER 13 /* Error state with retract*/
#define IS -1    /* Inavalid state */

/* State transition table definition */


#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */{ 1,4,6,1,1,IS,IS,IS },
	/* State 1 */{ 1,1,1,1,1,3,2,2 },
	/* State 2 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 3 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 4 */{ ES,4,4,ES,ES,5,7,5 },
	/* State 5 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 6 */{ ES,ES,5,9,ES,5,7,5 },
	/* State 7 */{ 8,7,7,8,8,8,8,8 },
	/* State 8 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 9 */{ ER,10,10,ER,10,ER,ER,ER },
	/* State 10 */{ ES,10,10,ES,10,11,11,11 },
	/* State 11 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 12 */{ IS,IS,IS,IS,IS,IS,IS,IS },
	/* State 13 */{ IS,IS,IS,IS,IS,IS,IS,IS }
};
/* Accepting state table definition */
#define ASWR     14  /* accepting state with retract */
#define ASNR     15  /* accepting state with no retract */
#define NOAS     16  /* not accepting state */

int as_table[] = { NOAS,NOAS,ASWR,ASNR,NOAS,ASWR,NOAS,NOAS,ASWR,NOAS,NOAS,ASWR,ASNR,ASWR };

/* Accepting action function declarations */

Token aa_func02(char *lexeme);/*AVID/KW*/
Token aa_func03(char *lexeme);/*SVID*/
Token aa_func05(char *lexeme);/*DIL/0*/
Token aa_func08(char *lexeme);/*FPL*/
Token aa_func11(char *lexeme);/*HIL*/
Token aa_func12(char *lexeme);/*Error State no retract*/
							  /*Token aa_func13(char *lexeme);*//*Error State with retract*/

																/* defining a new type: pointer to function (of one char * argument)
																returning Token
																*/

typedef Token(*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[] = {
	NULL,NULL,aa_func02,aa_func03,NULL,aa_func05,NULL,NULL,aa_func08,NULL,NULL,aa_func11,aa_func12,aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif

