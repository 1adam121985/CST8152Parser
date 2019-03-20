/*
* File Name: buffer.h
* Compiler: MS Visual Studio 2015
* Author: Adam White 040 518 422
* Course: CST 8152 - Compilers, Lab Section: 11
* Assignment: 1
* Date: 28 September 2017
* Professor: Sv. Ranev
* Purpose: Preprocessor directives, type declarations and prototypes necessary for buffer implementation.
* Function list:
*	b_isfull() as macro expansion
*	b_allocate()
*	b_addc()
*	b_clear()
*	b_free()
*	b_isfull()
*	b_limit()
*	b_capacity()
*	b_mark()
*	b_mode()
*	b_incfactor()
*	b_load()
*	b_isempty()
*	b_eob()
*	b_getc()
*	b_print()
*	b_compact()
*	b_rflag()
*	b_retract()
*	b_reset()
*	b_getcoffset()
*	b_rewind()
*	b_location()
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

							/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

														   /* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */
														   /* constant definitions */
														   /* You may add your own constant definitions here */
#define RT_FAIL1 -1/* fail return value */
#define RT_FAIL2 -2/* fail return value */
#define LOAD_FAIL -2/* load fail error */
#define SET_R_FLAG 1/* realloc flag set value */
#define SET_EOB 1/*end of buffer flag set value*/		
#define O_MODE_F 0/*value for fixed mode in mode descriptor of buffer*/
#define O_MODE_A 1/*value for additive mode in mode descriptor of buffer*/
#define O_MODE_M -1/*value for multiplicative mode in mode descriptor of buffer*/
#define O_MODE_F_INC_FACTOR 0/*Fixed mode inc_factor*/
#define O_MODE_A_MIN_INC_FACTOR 1/*minimum value for inc_factor in additive mode*/
#define O_MODE_A_MAX_INC_FACTOR 255/*maximum value for inc_factor in additive mode*/
#define O_MODE_M_MIN_INC_FACTOR 1 /*minimum value of inc_factor in operation mode 'm'*/
#define O_MODE_M_MAX_INC_FACTOR 100 /*maximum value of inc_factor in operation mode 'm'*/
#define MAX_BUFFER_CAPACITY (SHRT_MAX - 1)/*maximum buffer capacity measured in char elements*/
#define O_MODE_M_INC_FACTOR_DENOMINATOR 100/*denominator for inc_facotr in multiplicative mode*/
#define B_INCFACTOR_FAILURE 256/*return value for failure of b_infactor function*/
/*#define TRUE 1*//*true return value*/
/*#define FALSE 0*//*false return value*/

														   /*
														   * Purpose: Macro expansion evaulates whether buffer is full
														   * Author: Adam White
														   * History/Versions: 1.0 28 September 2017
														   * Called Functions:
														   * Parameters:
														   *	Buffer pBD
														   * Return Value:
														   *	int (TRUE,FALSE or RT_FAIL1 on error)
														   * Algorithm:
														   */
#define b_isfull(pBD) !pBD ? RT_FAIL1 : (pBD->addc_offset == pBD->capacity ? TRUE : FALSE)

#ifndef B_FULL/*will use function b_isfull as opposed to macro expansion if B_FULL not defined*/
#undef b_isfull
#endif





														   /* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
#ifndef B_FULL
int b_isfull(Buffer * const pBD);
#endif
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(Buffer * const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t  b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char * b_location(Buffer * const pBD, short loc_offset);
#endif

