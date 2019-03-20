/*
* File Name: buffer.c
* Compiler: MS Visual Studio 2015
* Author: Adam White 040 518 422
* Course: CST 8152 - Compilers, Lab Section: 11
* Assignment: 1
* Date: 28 September 2017
* Professor: Sv. Ranev
* Purpose: Utility funtions for Buffer data structure
* Function list:
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

#define _CRT_SECURE_NO_WARNINGS
#include "buffer.h"
/*
* Purpose: Allocates memory for new Buffer type and returns its pointer upon success or Null upon failure
* Author: Adam White
* History/Versions: 1.01 November 30, 2017
* Called Functions: calloc(), malloc()
* Parameters:
*	short init_capactiy
*	char inc_factor
*	char o_mode
* Return Value:
*	Buffer* (NULL on failure, pointer to new Buffer otherwise)
* Algorithm:
*	-Checks for valid paramaters
*	-Allocates memory for Buffer
*	-Initializes Buffer mode and inc_factor member based on parameters inc_factor and o_mode
*	-Allocates memory for cb_head character buffer member of Buffer type based on init_capacity parameter
*	-Initializes init_capacity member of Buffer type to init_capacity parameter
*	-Returns pointer to Buffer*
*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {
	Buffer * buffer_ptr = NULL;
	char * head;
	short inc, mode;
	if (init_capacity < 0 || init_capacity > MAX_BUFFER_CAPACITY) return NULL;/*check for valid init_capacity*/
	if (o_mode != 'a' && o_mode != 'f' && o_mode != 'm') return NULL;/*check for valid o_mode*/
	if (o_mode == 'f' || !inc_factor) {/*check o_mode and inc_factor in paramater and assign to respective Buffer members*/
		if (!init_capacity) return NULL;/*What's the point of a 0 capacity fixed buffer?*/
		mode = O_MODE_F;
		inc = O_MODE_F_INC_FACTOR;
	}
	else if (o_mode == 'a') {/*no need to check inc_factor, we know it's non-zero from previous check, hence between 1 and 255 when cast to unsigned char*/
		mode = O_MODE_A;
		inc = inc_factor;
	}
	else if (inc_factor >= O_MODE_M_MIN_INC_FACTOR && inc_factor <= O_MODE_M_MAX_INC_FACTOR) {/*o_mode=='m' implicitly*/
		mode = O_MODE_M;
		inc = inc_factor;
	}
	else return NULL; /*frees buffer_ptr and returns null if inc_factor invalid for o_mode m*/
	head = (char *)malloc(init_capacity * sizeof(char));/*allocate memory for character buffer*/
	if (!head) return NULL;/*return NULL on failure*/
	buffer_ptr = (Buffer *)calloc(1, sizeof(Buffer));/*Allocates memory for Buffer and initializes members to 0*/
	if (!buffer_ptr) { free(head); return NULL; }/*return NULL on failure*/
	buffer_ptr->capacity = init_capacity; /*sets capacity to init_capacity*/
	buffer_ptr->cb_head = head;
	buffer_ptr->inc_factor = (char)inc;
	buffer_ptr->mode = (char)mode;
	return buffer_ptr;
}
/*
* Purpose: Adds character to character buffer
* Author: Adam White
* History/Versions: 1.01 November 30, 2017
* Called Functions: realloc()
* Parameters:
*	pBuffer pBD (only NULL or pointer to Buffer)
*	char symbol
* Return Value:
*	pBuffer (NULL on failure, pBD otherwise)
* Algorithm:
*	-Checks for valid paramaters
*	-Adds symbol parameter to character buffer if not full and returns pBD parameter
*	-If full, resizes character buffer depending on operation mode and increment factor before adding symbol to buffer
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	short new_capacity;/*new character capacity of Buffer*/
	char* tmp = NULL;/*temporary storage for new character buffer pointer, needed to error check realloc and to compare to pBD->cb_head*/
	if (!pBD) return NULL;/*NULL pointer check*/
	pBD->r_flag = 0;
	if (pBD->addc_offset != pBD->capacity) {/*if not full, assign symbol, inc addc_offset and return*/
		pBD->cb_head[(pBD->addc_offset)++] = symbol;
		return pBD;
	}
	if (pBD->capacity == MAX_BUFFER_CAPACITY) return NULL;/*Can't add anything if full and at max capacity*/
	if (pBD->mode == O_MODE_F) return NULL;/*handles if full and mode f*/
	if (pBD->mode == O_MODE_A) {/*if full and mode a*/
		new_capacity = pBD->capacity + (unsigned char)pBD->inc_factor;/*attempts to increment additively*/
		if (new_capacity < 0) return NULL;/*exceeded bounds of signed data type*/
		if (new_capacity > MAX_BUFFER_CAPACITY) new_capacity = MAX_BUFFER_CAPACITY;/*if on the rare chance that new_capacity is the one number between MAX_BUFFER_CAPACITY and SHRT_MAX*/
	}
	else {/*if full and mode m*/
		new_capacity = (short)(((double)MAX_BUFFER_CAPACITY - (double)pBD->capacity)*((double)pBD->inc_factor / (double)O_MODE_M_INC_FACTOR_DENOMINATOR));
		new_capacity = !new_capacity ? MAX_BUFFER_CAPACITY : new_capacity + pBD->capacity;
	}
	tmp = (char *)realloc(pBD->cb_head, new_capacity);/*attempts to resize buffer to new capacity*/
	if (!tmp) return NULL;/*check realloc doesn't fail*/
	if (tmp != pBD->cb_head) /*set r_flag and assign new cb_head if mem location changes from realloc*/
		pBD->r_flag = SET_R_FLAG;
	else pBD->r_flag = 0;
	pBD->cb_head = tmp;
	pBD->capacity = new_capacity;/*assign new capacity*/
	pBD->cb_head[(pBD->addc_offset)++] = symbol;/*assign new symbol and increment addc_offset*/
	return pBD;
}
/*
* Purpose: Empties character buffer
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (0 on success, RT_FAIL1 on failure)
* Algorithm:
*/
int b_clear(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return pBD->eob = pBD->addc_offset = pBD->getc_offset = pBD->markc_offset = pBD->r_flag = 0;/*Return upon success not specified in specifications*/
}
/*
* Purpose: Frees memory allocated to Buffer
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	void
* Algorithm:
* -check paramater
* -free character array member cb_head
* -free Buffer
*/
void b_free(Buffer * const pBD) {
	if (pBD) {/*NULL pointer check, can't access pBD->cb_head if so*/
		free(pBD->cb_head);
		pBD->cb_head = NULL;
		free(pBD);
	}
}
/*
* Purpose: Evaulates whether buffer is full
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (TRUE,FALSE or RT_FAIL1 on error)
* Algorithm:
*/
#ifndef B_FULL/*Will omit b_isfull function and use macro expansion b_isfull defined in buffer.h if B_FULL defined*/
int b_isfull(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return pBD->addc_offset == pBD->capacity ? 1 : 0;
}
#endif
/*
* Purpose: Returns number of characters stored in buffer
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	short (RT_FAIL1 on failure, expected output otherwise)
* Algorithm:
*/
short b_limit(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return pBD->addc_offset;
}
/*
* Purpose: Returns buffer capactiy
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	short (RT_FAIL1 on failure, capacity otherwise)
* Algorithm:
*/
short b_capacity(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return pBD->capacity;
}
/*
* Purpose: Sets markc_offset member of Buffer type to parameter mark
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
*	short mark
* Return Value:
*	short (RT_FAIL1 on failure, new mark position otherwise)
* Algorithm:
*/
short b_mark(Buffer * const pBD, short mark) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	if (mark > pBD->addc_offset || mark < 0) return RT_FAIL1;
	return pBD->markc_offset = mark;
}
/*
* Purpose: Returns buffer mode
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (RT_FAIL2 on failure, O_MODE_A for additive mode, O_MODE_M for multiplicative mode, O_MODE_F for fixed mode)
* Algorithm:
*/
int b_mode(Buffer * const pBD) {
	if (!pBD) return RT_FAIL2;/*NULL pointer check*/
	return pBD->mode;
}
/*
* Purpose: Returns inc_factor
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	size_t (B_INCFACTOR_FAILURE on failure, inc_factor otherwise)
* Algorithm:
*/
size_t  b_incfactor(Buffer * const pBD) {
	if (!pBD) return B_INCFACTOR_FAILURE;/*NULL pointer check*/
	return (unsigned char)pBD->inc_factor;
}
/*
* Purpose: Loads into buffer contents of file
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions: feof(), fgetc(), b_addc()
* Parameters:
*	File* fi (only NULL or pointer to File)
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (RT_FAIL1 for non-loading failure, LOAD_FAIL for loading failure, number of characters loaded otherwise)
* Algorithm:
*	-Validate parameters
*	-Read file and load file contents into buffer using b_addc until b_addc fails or until end of file
*/
int b_load(FILE * const fi, Buffer * const pBD) {
	int tmp;/*temporarily stores return of fgetc for purpose of validation*/
	int counter = 0;/*counts number of characters added to buffer*/
	if (!fi || !pBD) return RT_FAIL1;/*NULL pointer check*/
	while (1) {/*reads file characters into buffer until load failure (returning -2 or LOAD_FAIL) or eof (returning numbers of chars added to buffer or counter)*/
		tmp = fgetc(fi);
		if (feof(fi)) return counter; /*check for end of file*/
		if (tmp == EOF) return RT_FAIL1;/*this isn't checking end of file because it was checked on last line, it's checking if fgetc failed*/
		if (!b_addc(pBD, (char)tmp)) return LOAD_FAIL;
		counter++;
	}
}
/*
* Purpose: Evaluates whether buffer is empty
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (RT_FAIL1 on failure, TRUE if empty, FALSE if not empty)
* Algorithm:
*/
int b_isempty(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return !pBD->addc_offset ? 1 : 0;
}
/*
* Purpose: Returns eob
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (RT_FAIL1 on failure, eob on success)
* Algorithm:
*/
int b_eob(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return pBD->eob;
}
/*
* Purpose: Returns next character in buffer
* Author: Adam White
* History/Versions: 1.01 November 30, 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (RT_FAIL2 if parameter invalid, RT_FAIL1 if buffer full, expected character otherwise)
* Algorithm:
*	-check for valid parameter
*	-set eob flag and return RT_FAIL1 if full
*	-return next character in buffer and increment getc_offset
*/
char b_getc(Buffer * const pBD) {
	if (!pBD) return RT_FAIL2;/*NULL ptr check*/
	if (pBD->getc_offset == pBD->addc_offset) { pBD->eob = SET_EOB; return RT_FAIL1; }/*flag if at end of buffer*/
	else return pBD->cb_head[(pBD->getc_offset)++];
}
/*
* Purpose: Prints contents of character buffer
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (RT_FAIL1 on failure, number of characters printed on success)
* Algorithm:
*	-check for valid parameter
*	-print contents of buffer until end of buffer reached
*/
int b_print(Buffer * const pBD) {
	short counter = 0;/*counter for number of characters printer*/
	if (!pBD) return RT_FAIL1;/*NULL ptr check*/
	if (!pBD->addc_offset) {/*empty buffer check*/
		printf("Empty buffer\n");
		return RT_FAIL1;
	}
	while (1) {/*loop prints buffer content and increments coutner until eob reached*/
		char tmp = b_getc(pBD);/*used within scope of loop to store b_getc.*/
		if (pBD->eob) break;/*tests for flag change from b_getc*/
		printf("%c", tmp);/*prints tmp if passes flag check*/
		counter++;
	}
	printf("\n");
	return counter;
}
/*
* Purpose: Resizes buffer to getc_offset + 1 and adds symbol to buffer
* Author: Adam White
* History/Versions: 1.01 November 30, 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	Buffer* (NULL on failure, pBD on success)
* Algorithm:
*	-check for valid parameters
*	-resize buffer
*	-add symbol to buffer
*/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	char * tmp = NULL;
	if (!pBD) return NULL;/*NULL pointer check*/
	if ((pBD->addc_offset + 1) < 0) return NULL;/*already at max for type short*/
	tmp = (char *)realloc(pBD->cb_head, (pBD->addc_offset + 1));/*attempts to realloc buffer to addc_offset+1*/
	if (!tmp) return NULL;/*return NULL if realloc failed*/
	if (tmp != pBD->cb_head)
		pBD->r_flag = SET_R_FLAG;
	else
		pBD->r_flag = 0;
	pBD->cb_head = tmp;
	pBD->cb_head[(pBD->addc_offset)++] = symbol;
	pBD->capacity = pBD->addc_offset;
	return pBD;
}
/*
* Purpose: Returns r_flag
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	char (RT_FAIL1 on failure, expected value otherwise)
* Algorithm:
*/
char b_rflag(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return pBD->r_flag;
}
/*
* Purpose: Decrements getc_offset and returns new getc_offset
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	short (RT_FAIL1 on failure, expected value otherwise)
* Algorithm:
*/
short b_retract(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	if (!pBD->getc_offset) return RT_FAIL1;
	return --(pBD->getc_offset);
}
/*
* Purpose: Resets getc_offset to previously marked position, markc_offset, or 0
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	short (RT_FAIL1 on failure, mark position otherwise)
* Algorithm:
*/
short b_reset(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return (pBD->getc_offset = pBD->markc_offset);
}
/*
* Purpose: returns getc_offset
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	short (RT_FAIL1 on failure, expected value otherwise)
* Algorithm:
*/
short b_getcoffset(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return pBD->getc_offset;
}
/*
* Purpose: Sets Buffer type members, eob, getc_offset and markc_offset to 0, hence the name
* Author: Adam White
* History/Versions: 1.0 28 September 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
* Return Value:
*	int (RT_FAIL1 on failure, 0 otherwise)
* Algorithm:
*/
int b_rewind(Buffer * const pBD) {
	if (!pBD) return RT_FAIL1;/*NULL pointer check*/
	return pBD->eob = pBD->getc_offset = pBD->markc_offset = 0;
}
/*
* Purpose: Returns pointer to character at position loc_offset in buffer
* Author: Adam White
* History/Versions: 1.01 November 30, 2017
* Called Functions:
* Parameters:
*	Buffer* pBD (only NULL or pointer to Buffer)
*	short loc_offset
* Return Value:
*	char* (NULL on failure, expected char* otherwise)
* Algorithm:
*/
char * b_location(Buffer * const pBD, short loc_offset) {
	if (!pBD) return NULL;/*NULL pointer check*/
	if (!pBD->cb_head) return NULL;
	if (loc_offset < 0 || loc_offset >= pBD->addc_offset) return NULL;
	return &(pBD->cb_head[loc_offset]);
}