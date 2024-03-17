#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <setjmp.h>
#include <ctype.h>
#include <math.h>

/* == configuration == */

/* 1 = use doubles
 * 0 = use floats */
#define SYLT_USE_DOUBLES 1

/* functions for printing to standard, error
 * and debug output, respectively */
#define sylt_printf printf
#define sylt_eprintf printf
#define sylt_dprintf printf

/* initial stack size */
#define SYLT_INIT_STACK 512

/* == debug flags ==
 * all of these should be set to 0 in
 * release builds */

/* enables assertions */
#define DBG_ASSERTIONS 1

/* disables garbage collection, though
 * memory will still be freed at shutdown
 * (sylt_free) to prevent memory leaks */
#define DBG_NO_GC 1

/* makes the GC collect on every allocation */
#define DBG_GC_EVERY_ALLOC 1

#define DBG_PRINT_SYLT_STATE 1
#define DBG_PRINT_GC_STATE 1
#define DBG_PRINT_SOURCE 0
#define DBG_PRINT_TOKENS 0
#define DBG_PRINT_NAMES 0
#define DBG_PRINT_AST 1
#define DBG_PRINT_CODE 1
#define DBG_PRINT_DATA 0
#define DBG_PRINT_STACK 0
#define DBG_PRINT_MEM_STATS 1
#define DBG_PRINT_MEM_SIZES 0

/* == optimizations == */

#define OPTZ_DEDUP_CONSTANTS 1
#define OPTZ_SPECIAL_PUSHOPS 1

/* == constant limits == */

#define MAX_CODE (UINT16_MAX + 1)
#define MAX_DATA (UINT8_MAX + 1)
#define MAX_STACK (UINT8_MAX + 1)
#define MAX_LINES UINT32_MAX
#define MAX_JUMP UINT16_MAX
#define MAX_CFRAMES 64
#define MAX_PARAMS UINT8_MAX
#define MAX_UPVALUES UINT8_MAX
#define MAX_MATCH_ARMS 512
#define MAX_ERRMSGLEN 1024

/* == debug macros == */

#if DBG_ASSERTIONS
#define dbgerr(msg) \
	sylt_dprintf("%s in %s:%d", \
		(msg), __FILE__, __LINE__); \
	exit(EXIT_FAILURE);
#define assert(cond) \
	if (!(cond)) { \
		dbgerr("assertion failed"); \
	}
#define unreachable() \
	dbgerr("unreachable code entered");
#else
#define dbgerr(msg) (void)msg
#define assert(cond) (void)cond
#define unreachable()
#endif

/* == sylt context == */

typedef enum {
	/* uninitialized but allocated state */
	SYLT_STATE_ALLOC,
	/* context initialized */
	SYLT_STATE_INIT,
	
	/* compiling input */
	SYLT_STATE_COMPILING,
	/* done compiling */
	SYLT_STATE_COMPILED,
	
	/* executing a program */
	SYLT_STATE_EXEC,
	/* execution finished */
	SYLT_STATE_DONE,
	
	/* sylt_free() called */
	SYLT_STATE_FREEING,
	/* context freed; all pointers invalid */
	SYLT_STATE_FREE,
} sylt_state_t;

static const char* SYLT_STATE_NAME[] = {
	"Alloc",
	"Init",
	"Compiling",
	"Compiled",
	"Exec",
	"Done",
	"Freeing",
	"Free",
};

typedef enum {
	/* not busy */
	GC_STATE_IDLE,
	/* marking reachable objects */
	GC_STATE_MARK,
	/* deallocating unreachable objects */
	GC_STATE_SWEEP,
	/* unable to collect in this state */
	GC_STATE_PAUSED,
} gc_state_t;

typedef struct {
	gc_state_t state;
	/* array of pointers to marked objects */
	struct obj_s** marked;
	size_t nmarked;
	/* number of succesful gc_collect calls */
	size_t nruns;
	/* how many times the GC was paused */
	size_t npauses;
} gc_t;

typedef struct {
	/* linked list of all objects */
	struct obj_s* objs;
	/* current allocation size */
	ptrdiff_t bytes;
	/* highest usage at any one time */
	ptrdiff_t highest;
	/* total number of allocations */
	size_t count;
	/* garbage collector */
	gc_t gc;
} mem_t;

/* sylt API struct */
typedef struct {
	sylt_state_t state;
	/* virtual machine state */
	struct vm_s* vm;
	/* compiler state; note that this
	 * is only a valid pointer while
	 * state == STATE_COMPILING */
	struct comp_s* cmp;
	/* memory */
	mem_t mem;
} sylt_t;

/* longjmp destination on error.
 * kept outside sylt_t struct as an
 * out of memory error could occur
 * while allocating it */
static jmp_buf err_jump;

static void sylt_set_state(
	sylt_t* ctx, sylt_state_t state)
{
	ctx->state = state;
	
	#if DBG_PRINT_SYLT_STATE
	sylt_dprintf("[state: %s]\n",
		SYLT_STATE_NAME[state]);
	#endif
}

/* == error messages == */

void halt(sylt_t*, const char*, ...);

#define E_OUTOFMEM \
	"out of memory"
#define E_CODELIMIT \
	"bytecode limit (%d) reached", MAX_CODE
#define E_DATALIMIT \
	"data limit (%d) reached", MAX_DATA
#define E_STACKLIMIT \
	"stack limit (%d) reached", MAX_STACK
#define E_LINELIMIT \
	"line limit (%d) reached", MAX_LINES
#define E_JUMPLIMIT \
	"jump distance too far (>%d)", MAX_JUMP
#define E_IO(msg, path) \
	"%s %s", (msg), (path)
#define E_UNEXPECTEDCHAR(c) \
	"unknown character '%c'", (c)
#define E_TYPE(ex, got) \
	"expected %s but got %s", \
	user_type_name(ex), \
	user_type_name(got)
#define E_UNDEFINED(name, len) \
	"undefined variable '%.*s'", \
	(name), (len)
#define E_ESCAPESEQ(code) \
	"unknown escape sequence '\\%c'", \
	(code)
#define E_UNTERMSTRING \
	"unterminated string literal"
#define E_TOOMANYPARAMS \
	"too many parameters; limit is %d", \
	(MAX_PARAMS)
#define E_TOOMANYARGS \
	"too many arguments; limit is %d", \
	(MAX_PARAMS)
#define E_TOOMANYUPVALUES \
	"too many upvalues; max is %d", \
	(MAX_UPVALUES)

/* == runtime errors == */

#define E_DIVBYZERO \
	"attempted to divide by zero"
#define E_STACKOVERFLOW \
	"call stack overflow"
#define E_INDEX(len, index) \
	"index out of range, len: %d, i=%d", \
	(len), (index)
#define E_WRONGARGC(need, got) \
	"expected %d arguments but got %d", \
	(need), (got)

/* == memory == */

/* returns n rounded up to the nearest
 * power of two */
static unsigned nextpow2(unsigned n) {
	n--;
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n |= n >> 8;
	n |= n >> 16;
	return ++n;
}

#define gc_realloc realloc
#define gc_free free

void gc_pause(sylt_t*);
void gc_resume(sylt_t*);
void gc_collect(sylt_t*, const char*, int);

void* ptr_resize(
	void* p,
	size_t os,
	size_t ns,
	const char* p_name,
	const char* func_name,
	int line,
	sylt_t* ctx)
{
	assert(ns >= 0);
	assert(os >= 0);
	
	/* keep track of heap size.
	 * the null check is for when we
	 * allocate the ctx struct itself */
	if (ctx) {
		ctx->mem.bytes += ns - os;
		
		if (ctx->mem.bytes > ctx->mem.highest)
			ctx->mem.highest = ctx->mem.bytes;
				
		ctx->mem.count++;
		
		#if DBG_GC_EVERY_ALLOC
		if (ns > os)
			gc_collect(ctx, func_name, line);
		#endif
	}
	
	#if DBG_PRINT_GC_STATE
	if (ns > 0)
		sylt_dprintf(
			"        %+ld to %s in %s:%d\n",
			ns - os, p_name, func_name, line);
	#endif
	
	if (ns == 0) {
		free(p);
		return NULL;
	}
	
	void* np = realloc(p, ns);
	if (!np) {
		halt(ctx, E_OUTOFMEM);
	    return NULL;
	}
	
	return np;
}

/* useful macros for allocating pointers */
#define ptr_alloc(p, t, ctx) \
	((p) = ptr_resize(NULL, 0, sizeof(t), \
		#p, __func__, __LINE__, ctx))
#define ptr_free(p, t, ctx) \
	ptr_resize(p, sizeof(t), 0, \
		#p, __func__, __LINE__, ctx)

/* macros for handling dynamic arrays */
#define arr_alloc(p, t, n, ctx) \
	((p) = ptr_resize( \
		NULL, 0, sizeof(t) * n, \
		#p, __func__, __LINE__, ctx))
#define arr_resize(p, t, os, ns, ctx) \
	ptr_resize(p, \
		sizeof(t) * (os), \
		sizeof(t) * (ns), \
		#p, __func__, __LINE__, ctx)
#define arr_free(p, t, os, ctx) \
	ptr_resize(p, sizeof(t) * (os), 0,\
		#p, __func__, __LINE__, ctx)

/* == opcodes == */

typedef enum {
	/* stack */
	OP_PUSH,
	OP_PUSH_NIL,
	OP_PUSH_TRUE,
	OP_PUSH_FALSE,
	OP_PUSH_LIST,
	OP_PUSH_FUNC,
	OP_POP,
	OP_DUP,
	OP_SWAP,
	OP_LOAD,
	OP_STORE,
	OP_LOAD_LIST,
	OP_LOAD_UPVAL,
	OP_MOVE_HEAP,
	OP_HIDE_RET,
	OP_SHOW_RET,
	/* arithmetic */
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_EDIV,
	OP_UMIN,
	/* comparison */
	OP_LT,
	OP_LTE,
	OP_GT,
	OP_GTE,
	/* equality */
	OP_EQ,
	OP_NEQ,
	OP_NOT,
	/* control flow */
	OP_JMP,
	OP_JMPIF,
	OP_JMPIFN,
	OP_CALL,
	OP_RET,
} op_t;

/* info about an opcode, used by
 * compiler and for debugging */
typedef struct {
	const char* name;
	/* how many arg bytes an op takes */
	int rank;
	/* by how much the op modifies the stack,
 	* used to statically compute the
 	* stack size needed for each chunk */
	int effect;
} opinfo_t;

/* can be indexed by an opcode byte */
static opinfo_t OPINFO[] = {
	[OP_PUSH] = {"push", 1, +1},
	[OP_PUSH_NIL] = {"push nil", 0, +1},
	[OP_PUSH_TRUE] = {"push true", 0, +1},
	[OP_PUSH_FALSE] = {"push false", 0, +1},
	[OP_PUSH_LIST] = {"push list", 1, +1},
	[OP_PUSH_FUNC] = {"push func", 1, +1},
	[OP_POP] = {"pop", 0, -1},
	[OP_DUP] = {"dup", 0, +1},
	[OP_SWAP] = {"swap", 0, 0},
	[OP_LOAD] = {"load", 1, +1},
	[OP_STORE] = {"store", 1, 0},
	[OP_LOAD_LIST] = {"load list", 0, -1},
	[OP_LOAD_UPVAL] = {"load upval", 1, +1},
	[OP_MOVE_HEAP] = {"move heap", 0, -1},
	[OP_HIDE_RET] = {"hide ret", 0, -1},
	[OP_SHOW_RET] = {"show ret", 0, +1},
	[OP_ADD] = {"add", 0, -1},
	[OP_SUB] = {"sub", 0, -1},
	[OP_MUL] = {"mul", 0, -1},
	[OP_DIV] = {"div", 0, -1},
	[OP_EDIV] = {"ediv", 0, -1},
	[OP_UMIN] = {"umin", 0, 0},
	[OP_LT] = {"lt", 0, -1},
	[OP_LTE] = {"lte", 0, -1},
	[OP_GT] = {"gt", 0, -1},
	[OP_GTE] = {"gte", 0, -1},
	[OP_EQ] = {"eq", 0, -1},
	[OP_NEQ] = {"neq", 0, -1},
	[OP_NOT] = {"not", 0, 0},
	[OP_JMP] = {"jmp", 2, 0},
	[OP_JMPIF] = {"jmpif", 2, 0},
	[OP_JMPIFN] = {"jmpifn", 2, 0},
	[OP_CALL] = {"call", 1, 0},
	[OP_RET] = {"ret", 0, 0},
};

/* value types */
typedef enum {
	TYPE_NIL,
	TYPE_BOOL,
	TYPE_NUM,
	TYPE_LIST,
	TYPE_STRING,
	TYPE_FUNCTION,
	/* the types below are for internal
	 * use only and should not be exposed
	 * to the user */
	TYPE_CLOSURE,
	TYPE_UPVALUE,
} type_t;

static const char* TYPE_NAMES[] = {
	"Nil",
	"Bool",
	"Num",
	"List",
	"String",
	"Function",
	"Closure",
	"Upvalue",
};

static const char* user_type_name(
	type_t tag)
{
	switch (tag) {
	case TYPE_CLOSURE: return "Function";
	case TYPE_FUNCTION:
	case TYPE_UPVALUE: unreachable();
	default: return TYPE_NAMES[tag];
	}
}

#define isheaptype(t) \
	((t) == TYPE_LIST \
		|| (t) == TYPE_STRING \
		|| (t) == TYPE_FUNCTION \
		|| (t) == TYPE_CLOSURE \
		|| (t) == TYPE_UPVALUE)

/* heap-allocated object header */
typedef struct obj_s {
	type_t tag;
	/* marked as reachable */
	bool marked;
	/* linked list of all objects */
	struct obj_s* next;
} obj_t;

#if SYLT_USE_DOUBLES
typedef double sylt_num_t;
#else
typedef float sylt_num_t;
#endif

/* tagged enum representing a Sylt value */
typedef struct value_s {
	type_t tag;
	union {
		sylt_num_t num;
		obj_t* obj;
	} data;
} value_t;

/* list object */
typedef struct {
	obj_t obj;
	value_t* items;
	size_t len;
} list_t;

/* string object */
typedef struct string_s {
	obj_t obj;
	/* list of characters, not required
	 * to be zero-terminated */
	uint8_t* bytes;
	size_t len;
} string_t;

/* function pointer to a sylt library
 * function written in C*/
typedef struct value_s (*cfunc_t)
	(sylt_t* ctx);

/* function object */
typedef struct {
	obj_t obj;
	
	/* bytecode */
	uint8_t* code;
	size_t ncode;
	/* constant data; stores any
	 * literal values found in
	 * the source code */
	value_t* data;
	size_t ndata;
	/* line numbers mapped to bytes */
	uint32_t* lines;
	size_t nlines;
	
	/* total stack slots needed */
	size_t slots;
	/* number of parameters */
	int params;
	/* number of upvalues */
	int upvalues;
	
	/* name, used in error messages and
	 * for debugging */
	string_t* name;
	/* if set this calls a C function
	 * and the bytecode is unused */
	cfunc_t cfunc;
} func_t;

/* closure object;
 * wraps a function and captures its
 * surrounding context, for example a
 * variable declared outside of, but
 * referenced inside the function body */
typedef struct {
	obj_t obj;
	const func_t* func;
	struct upvalue_s** upvals;
	size_t nupvals;
} closure_t;

/* upvalue object;
 * used to keep a reference a value outside
 * of a functions local stack window */
typedef struct upvalue_s {
	obj_t obj;
	/* stack index of live value on stack,
	 * or -1 if closed */
	int index;
	/* if the value in the above slot goes
	 * off the stack it gets copied to here */
	value_t closed;
	struct upvalue_s* next;
} upvalue_t;

/* call frame; execution state relative
 * to the last function call */
typedef struct {
	/* prototype of called function */
	const func_t* func;
	/* closure wrapping the prototype */
	const closure_t* cls;
	/* points to the current instruction */
	uint8_t* ip;
	/* offset within global stack */
	size_t offs;
} cframe_t;

/* VM execution state */
typedef struct vm_s {
	/* value stack */
	value_t* stack;
	size_t maxstack;
	/* points to top of stack + 1 */
	value_t* sp;
	
	/* call stack */
	cframe_t frames[MAX_CFRAMES];
	size_t nframes;
	cframe_t* fp;
	
	/* linked list of open upvalues */
	upvalue_t* openups;
	
	/* special slot for return value */
	value_t hidden;
	
	/* reference to API */
	sylt_t* ctx;
} vm_t;

/* selects a function depending on whether
 * sylt_num_t is a float or a double */
#if SYLT_USE_DOUBLES
#define num_func(flt, dbl) dbl
#else
#define num_func(flt, dbl) flt
#endif

/* macros for creating a value_t
 * struct from a raw value */
#define wrapnil() \
	(value_t){TYPE_NIL, {.num = 0}}
#define wrapbool(v) \
	(value_t){TYPE_BOOL, {.num = (v)}}
#define wrapnum(v) \
	(value_t){TYPE_NUM, {.num = (v)}}
#define wraplist(v) \
	(value_t){TYPE_LIST, \
		{.obj = (obj_t*)(v)}}
#define wrapstring(v) \
	(value_t){TYPE_STRING, \
		{.obj = (obj_t*)(v)}}
#define wrapfunc(v) \
	(value_t){TYPE_FUNCTION, \
		{.obj = (obj_t*)(v)}}
#define wrapclosure(v) \
	(value_t){TYPE_CLOSURE, \
		{.obj = (obj_t*)(v)}}

/* macros for getting the raw value from 
 * a value_t. the type must be
 * checked before accessing these to
 * prevent undefined behaviour */
#define getbool(v) (v).data.num
#define getnum(v) (v).data.num
#define getobj(v) (v).data.obj
#define getlist(v) \
	((list_t*)(v).data.obj)
#define getstring(v) \
	((string_t*)(v).data.obj)
#define getfunc(v) \
	((func_t*)(v).data.obj)
#define getclosure(v) \
	((closure_t*)(v).data.obj)

#define typecheck(ctx, v, t) \
	if (v.tag != t) \
		halt(ctx, E_TYPE(t, v.tag))
#define typecheck2(ctx, a, b, t) \
	typecheck(ctx, a, t); \
	typecheck(ctx, b, t)
	
/* == public stack API == */
/* these are useful even outside of the VM,
 * for making objects visible to the GC */

static inline void vm_push(vm_t*, value_t);
static inline value_t vm_pop(vm_t*);
static inline value_t vm_peek(
	const vm_t* vm, int n);
static inline void vm_shrink(vm_t*, int);

/* for pushing values on the stack */
#define sylt_push(ctx, v) \
	vm_push(ctx->vm, v)
#define sylt_pushstring(ctx, v) \
	sylt_push(ctx, wrapstring(v))
#define sylt_pushfunc(ctx, v) \
	sylt_push(ctx, wrapfunc(v))

/* for popping values off the stack */
#define sylt_pop(ctx) \
	vm_pop(ctx->vm)
#define sylt_popstring(ctx) \
	getstring(sylt_pop(ctx))
#define sylt_popfunc(ctx) \
	getfunc(sylt_pop(ctx))

/* for peeking values down the stack */
#define sylt_peek(ctx, n) \
	vm_peek(ctx->vm, n)
#define sylt_peekstring(ctx, n) \
	getstring(sylt_peek(ctx, n))
#define sylt_peekfunc(ctx, n) \
	getfunc(sylt_peek(ctx, n))

/* shrinks the stack by n values */
#define sylt_shrink(ctx, n) \
	vm_shrink(ctx->vm, n)
	
void dbg_print_obj_mem(
	obj_t* obj, bool freed)
{
	#if DBG_PRINT_GC_STATE
	if (!obj)
		return;
	
	sylt_dprintf("        %s%s %p\n",
		(freed) ? "-" : "+",
		TYPE_NAMES[obj->tag],
		obj);
	#endif
}

/* allocates a generic object */
obj_t* obj_new_impl(
	size_t size,
	type_t tag,
	const char* func_name,
	int line,
	sylt_t* ctx)
{
	assert(isheaptype(tag));
	//gc_collect(ctx, __func__, __LINE__);
	
	obj_t* obj = ptr_resize(
		NULL, 0, size,
		"<obj>", func_name, line,ctx);
	obj->tag = tag;
	obj->marked = false;
	obj->next = ctx->mem.objs;
	ctx->mem.objs = obj;
	
	dbg_print_obj_mem(obj, false);
	return obj;
}

#define obj_new(size, tag, ctx) \
	obj_new_impl(size, tag, \
		__func__, __LINE__, ctx);

/* frees a generic object and its
 * associated memory */
void obj_free(obj_t* obj, sylt_t* ctx) {
	if (!obj)
		return;
		
	assert(isheaptype(obj->tag));
	dbg_print_obj_mem(obj, true);
	
	switch (obj->tag) {
	case TYPE_LIST: {
		list_t* list = (list_t*)obj;
		arr_free(list->items,
			value_t,
			list->len,
			ctx);
		
		ptr_free(list, list_t, ctx);
		break;
	}
	case TYPE_STRING: {
		string_t* str = (string_t*)obj;
		arr_free(str->bytes,
			uint8_t,
			str->len,
			ctx);
		
		ptr_free(str, string_t, ctx);
		break;
	}
	case TYPE_FUNCTION: {
		func_t* func = (func_t*)obj;
		arr_free(func->code,
			uint8_t,
			nextpow2(func->ncode),
			ctx);
    	arr_free(func->data,
    		value_t,
    		nextpow2(func->ndata),
    		ctx);
    	arr_free(func->lines,
    		uint32_t,
    		nextpow2(func->nlines),
    		ctx);
		
		ptr_free(func, func_t, ctx);
		break;
	}
	case TYPE_CLOSURE: {
		closure_t* cls = (closure_t*)obj;
		arr_free(
			cls->upvals,
			upvalue_t*,
			cls->nupvals,
			ctx);
		
		ptr_free(cls, closure_t, ctx);
		break;
	}
	case TYPE_UPVALUE: {
		ptr_free(obj, upvalue_t, ctx);
		break;
	}
	default: unreachable();
	}
}

/* marks an object as reachable by the
 * garbage collector */
void obj_mark(obj_t* obj, sylt_t* ctx) {
	assert(obj);
	if (obj->marked)
		return;
	
	/* set a mark bit on the object itself */
	obj->marked = true;

	/* add the object to a cache for
	 * performance */
	ctx->mem.gc.marked = gc_realloc(
		ctx->mem.gc.marked,
		(sizeof(obj_t*)
			* ctx->mem.gc.nmarked) + 1);
	
	if (!ctx->mem.gc.marked)
		halt(ctx, E_OUTOFMEM);
	
	ctx->mem.gc.marked
		[ctx->mem.gc.nmarked++] = obj;
}

void val_mark(value_t val, sylt_t* ctx);

/* marks all references an object holds to
 * other objects */
void obj_deep_mark(obj_t* obj, sylt_t* ctx) {
	assert(obj);
	assert(isheaptype(obj->tag));
	assert(obj->marked);
	
	switch (obj->tag) {
	case TYPE_LIST: {
		list_t* ls = (list_t*)obj;
		for (size_t i = 0; i < ls->len; i++)
			val_mark(ls->items[i], ctx);
		
		break;
	}
	case TYPE_STRING: {
		break;
	}
	case TYPE_FUNCTION: {
		func_t* func = (func_t*)obj;
		for (size_t i = 0;
			i < func->ndata; i++)
			val_mark(func->data[i], ctx);
			
		obj_mark((obj_t*)func->name, ctx);
		break;
	}
	case TYPE_CLOSURE: {
		closure_t* cls = (closure_t*)obj;
		
		obj_mark((obj_t*)cls->func, ctx);
		for (size_t i = 0;
			i < cls->nupvals; i++)
			obj_mark((obj_t*)cls->upvals[i],
				ctx);
		
		break;
	}
	case TYPE_UPVALUE: {
		upvalue_t* upval = (upvalue_t*)obj;
		val_mark(upval->closed, ctx);
		break;
	}
	default: unreachable();
	}
}

/* == list == */

/* creates an empty list */
list_t* list_new(sylt_t* ctx) {
	list_t* ls = (list_t*)obj_new(
		sizeof(list_t), TYPE_LIST, ctx);
	ls->items = NULL;
	ls->len = 0;
	return ls;
}

/* returns the item at the given index */
value_t list_get(
	const list_t* ls, int index, sylt_t* ctx)
{
	/* allow negative indices */
	if (index < 0) {
		int mod = ls->len + index;
		if (mod > ls->len - 1) {
			halt(ctx, E_INDEX(ls->len, mod));
			unreachable();
		}
		return ls->items[mod];
	}
	
	if (index > ls->len - 1) {
		halt(ctx, E_INDEX(ls->len, index));
		unreachable();
	}
	return ls->items[index];
}

/* appends an item to the end of the list */
void list_push(
	list_t* ls, value_t val, sylt_t* ctx)
{
	ls->items = arr_resize(
		ls->items,
		value_t,
		ls->len,
		ls->len + 1,
		ctx);
	ls->items[ls->len++] = val;
}

/* deletes and returns the last item
 * in the list */
value_t list_pop(list_t* ls, sylt_t* ctx) {
	if (ls->len == 0)
		return wrapnil();
	value_t last = list_get(ls, -1, ctx);
	ls->items = arr_resize(
		ls->items,
		value_t,
		ls->len,
		ls->len - 1,
		ctx);
	ls->len--;
	return last;
}

/* == string == */

/* creates a new string
 * if [take] is true we take ownership of the
 * buffer, otherwise we create a copy */
string_t* string_new(
	const char* bytes,
	size_t len,
	bool take,
	sylt_t* ctx)
{
	string_t* str = (string_t*)obj_new(
		sizeof(string_t), TYPE_STRING, ctx);
		
	if (take) {
		/* our memory now */
		str->bytes = (uint8_t*)bytes;
		
	} else {
		if (len == 0) {
			str->bytes = NULL;
			str->len = len;
			return str;
		}
		
		/* hide from GC */
		sylt_pushstring(ctx, str);
		
		/* allocate our own buffer... */
		arr_alloc(str->bytes,
			uint8_t, len, ctx);
		
		/* ... and copy the provided string */
		if (bytes)
			memcpy(str->bytes, bytes, len);
		
		/* safe */
		sylt_popstring(ctx);
	}
	
	str->len = len;
	return str;
}

/* creates an empty string with length 0 */
string_t* string_empty(sylt_t* ctx) {
	return string_new(NULL, 0, false, ctx);
}

/* helper function for creating a new 
 * string object from a C string literal */
string_t* string_lit(
	const char* lit, sylt_t* ctx)
{
	return string_new(
		lit, strlen(lit), false, ctx);
}

/* creates a new formatted string */
string_t* string_fmt(
	sylt_t* ctx, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	size_t len = vsnprintf(
		NULL, 0, fmt, args);
		
	char* bytes = NULL;
	arr_alloc(bytes, char, len, ctx);
	vsnprintf(bytes, len + 1, fmt, args);
	va_end(args);
	
	return string_new(
		bytes, len, true, ctx);
}

/* concatenates two strings */
string_t* string_concat(
	const string_t* a,
	const string_t* b,
	sylt_t* ctx)
{
	size_t len = a->len + b->len;
	
	uint8_t* bytes = NULL;
	arr_alloc(bytes, uint8_t, len, ctx);
	memcpy(bytes, a->bytes, a->len);
	memcpy(bytes + a->len, b->bytes, b->len);
	
	string_t* result = string_new(
		(const char*)bytes,
		len, true, ctx);
	return result;
}

void sylt_concat(sylt_t* ctx) {
	assert(sylt_peek(ctx, 0).tag
		== TYPE_STRING);
	assert(sylt_peek(ctx, 1).tag
		== TYPE_STRING);
	
	string_t* res = string_concat(
		sylt_peekstring(ctx, 1),
		sylt_peekstring(ctx, 0),
		ctx);
		
	/* pop strings */
	sylt_shrink(ctx, 2);
	
	sylt_pushstring(ctx, res);
}

/* appends src to dst */
void string_append(
	string_t** dst,
	const string_t* src,
	sylt_t* ctx)
{
	sylt_pushstring(ctx, *dst);
	sylt_pushstring(ctx, src);
	sylt_concat(ctx);
	*dst = sylt_popstring(ctx);
}

/* == function == */

/* creates a new function */
func_t* func_new(
	sylt_t* ctx, string_t* name)
{
	func_t* func = (func_t*)obj_new(
		sizeof(func_t), TYPE_FUNCTION, ctx);
	
	func->code = NULL;
	func->ncode = 0;
	func->data = NULL;
	func->ndata = 0;
	func->lines = NULL;
	func->nlines = 0;
	
	func->slots = 0;
	func->params = 0;
	func->upvalues = 0;
	
	func->name = name;
	func->cfunc = NULL;
	return func;
}

/* writes a byte to the functions
 * bytecode array */
void func_write(
	func_t* func,
	uint8_t byte,
	uint32_t line,
	sylt_t* ctx)
{
	if (func->ncode >= MAX_CODE) {
		halt(ctx, E_CODELIMIT);
		return;
	}
	
	size_t oldsz = nextpow2(func->ncode);
	size_t newsz = nextpow2(func->ncode + 1);
	if (newsz > oldsz) {
		func->code = arr_resize(
			func->code,
			uint8_t,
			oldsz,
			newsz,
			ctx);
	}
	func->code[func->ncode++] = byte;
	
	/* map line number to byte */
	oldsz = nextpow2(func->nlines);
	newsz = nextpow2(func->nlines + 1);
	if (newsz > oldsz) {
		func->lines = arr_resize(
			func->lines,
			uint32_t,
			oldsz,
			newsz,
			ctx);
	}
	func->lines[func->nlines++] = line;
}

bool val_eq(value_t a, value_t b);

/* writes a value to a functions constant
 * data table and returns its index */
size_t func_write_data(
	func_t* func, value_t val, sylt_t* ctx)
{
	#if OPTZ_DEDUP_CONSTANTS
	/* check if a constant with the same
	 * value already exists and if so
	 * return its index */
	for (size_t i = 0; i < func->ndata; i++)
		if (val_eq(func->data[i], val))
			return i;
	#endif
	
	if (func->ndata >= MAX_DATA) {
		halt(ctx, E_DATALIMIT);
		unreachable();
	}
	
	vm_push(ctx->vm, val); /* GC */
	
	size_t oldsz = nextpow2(func->ndata);
	size_t newsz = nextpow2(func->ndata + 1);
	if (newsz > oldsz) {
		func->data = arr_resize(
			func->data,
			value_t,
			oldsz,
			newsz,
			ctx);
	}
	
	vm_pop(ctx->vm); /* GC */
		
	func->data[func->ndata++] = val;
	return func->ndata - 1;
}

/* == closure == */

/* creates a new closure around a function */
closure_t* closure_new(
	sylt_t* ctx, const func_t* func)
{
	closure_t* cls = (closure_t*)obj_new(
		sizeof(closure_t), TYPE_CLOSURE, ctx);
	cls->func = func;
	cls->upvals = NULL;
	cls->nupvals = func->upvalues;
	return cls;
}

/* == upvalue == */

/* creates a new upvalue */
upvalue_t* upvalue_new(
	sylt_t* ctx, size_t index)
{
	upvalue_t* upval = (upvalue_t*)obj_new(
		sizeof(upvalue_t), TYPE_UPVALUE, ctx);
	upval->index = index;
	upval->closed = wrapnil();
	upval->next = NULL;
	return upval;
}

/* == value == */

/* marks a value as reachable in case it
 * wraps an object */
void val_mark(value_t val, sylt_t* ctx) {
	if (!isheaptype(val.tag))
		return;
	obj_mark(getobj(val), ctx);
}

/* returns true if the two values are equal */
bool val_eq(value_t a, value_t b) {
	if (a.tag != b.tag)
		return false;
	
	switch (a.tag) {
	case TYPE_NIL:
		return true;
	case TYPE_BOOL:
		return getbool(a) == getbool(b);
	case TYPE_NUM:
		return getnum(a) == getnum(b);
	case TYPE_LIST: {
		list_t* lsa = getlist(a);
		list_t* lsb = getlist(b);
		if (lsa == lsb)
			return true;
		
		if (lsa->len != lsb->len)
			return false;
		
		for (int i = 0; i < lsa->len; i++) {
			if (!val_eq(
				lsa->items[i],
				lsb->items[i]))
				return false;
		}
		
		return true;
	}
	case TYPE_STRING: {
		string_t* stra = getstring(a);
		string_t* strb = getstring(b);
		if (stra == strb)
			return true;
		
		return stra->len == strb->len
			&& memcmp(
				stra->bytes,
				strb->bytes,
				stra->len) == 0;
	}
	case TYPE_FUNCTION:
	case TYPE_CLOSURE:
	case TYPE_UPVALUE: {
		return getobj(a) == getobj(b);
	}
	default: unreachable();
	}
}

string_t* val_tostring_opts(
	value_t, bool, int, sylt_t*);

/* converts a value to a printable string */
static string_t* val_tostring(
	value_t val, sylt_t* ctx)
{
	switch (val.tag) {
	case TYPE_NIL: {
		return string_lit("nil", ctx);
	}
	case TYPE_BOOL: {
		if (getbool(val))
			return string_lit("true", ctx);
		else
			return string_lit("false", ctx);
	}
	case TYPE_NUM: {
		return string_fmt(
			ctx, "%g", getnum(val));
	}
	case TYPE_LIST: {
		string_t* str = string_lit("[", ctx);
		sylt_pushstring(ctx, str); /* GC */
		
		/* print items */
		list_t* ls = getlist(val);
		for (size_t i = 0; i < ls->len; i++) {
			string_t* vstr =
				val_tostring_opts(
					ls->items[i],
					true,
					0,
					ctx);
			string_append(&str, vstr, ctx);
			
			/* add ', ' between items */
			if (i < ls->len - 1) {
				string_append(
					&str,
					string_lit(", ", ctx),
					ctx);
			}
		}
		
		string_t* close =
			string_lit("]", ctx);
		
		string_t* result = string_concat(
			str, close, ctx);
		
		sylt_pop(ctx); /* GC */
		return result;
	}
	case TYPE_STRING: {
		return getstring(val);
	}
	case TYPE_CLOSURE: {
		return
			getclosure(val)->func->name;
		/*string_t* suffix =
			string_lit("()", ctx);
		
		return string_concat(
			getclosure(val)->func->name,
			suffix,
			ctx);*/
	}
	default: unreachable();
	}
}

string_t* val_tostring_opts(
	value_t val,
	bool quotestr,
	int maxlen,
	sylt_t* ctx)
{
	string_t* vstr = val_tostring(val, ctx);
	
	if (quotestr && val.tag == TYPE_STRING) {
		string_t* str = string_empty(ctx);
		
		int len = vstr->len;
		bool shortened = false;
		
		if (maxlen > 0 && len > maxlen) {
			len = maxlen;
			shortened = true;
		}
		
		string_append(
			&str,
			string_fmt(
				ctx,
				"\"%.*s\"", len, vstr->bytes),
			ctx);
		
		if (shortened)
			string_append(
				&str,
				string_fmt(
					ctx,
					"(..+%ld)",
					str->len - maxlen),
				ctx);
		
		return str;
	}
	
	return vstr;
}

/* prints a value to stdout */
void val_print(
	value_t val,
	bool quotestr,
	int maxlen,
	sylt_t* ctx)
{
	string_t* str = val_tostring_opts(
		val, quotestr, maxlen, ctx);
	printf("%.*s", (int)str->len, str->bytes);
}

/* == virtual machine == */

void vm_ensurestack(vm_t*, int);

void vm_init(vm_t* vm, sylt_t* ctx) {
	vm->stack = NULL;
	vm->maxstack = 0;
	vm->sp = NULL;
	
	vm->nframes = 0;
	vm->fp = NULL;
	
	vm->openups = NULL;
	vm->hidden = wrapnil();
	vm->ctx = ctx;
	
	/* initialize some stack space */
	vm_ensurestack(vm, SYLT_INIT_STACK);
}

void vm_free(vm_t* vm) {
	arr_free(vm->stack, 
		value_t,
		vm->maxstack,
		vm->ctx);
}

void dbg_print_mem_stats(sylt_t* ctx) {
	#if DBG_PRINT_MEM_STATS
	sylt_dprintf(
		"[memory information]\n"
		"- leaked: %ld bytes\n"
		"- highest usage: %ld bytes\n"
		"- allocations: %ld\n"
		"- GC runs: %ld\n"
		"- GC pauses: %ld",
		ctx->mem.bytes - sizeof(sylt_t),
		ctx->mem.highest,
		ctx->mem.count,
		ctx->mem.gc.nruns,
		ctx->mem.gc.npauses);
	sylt_dprintf("\n");
	#endif
}

void dbg_print_mem_sizes(void) {
	#if DBG_PRINT_MEM_SIZES
	#define print_size(t) \
		sylt_dprintf(" %s = %ld\n", \
			#t, sizeof(t))
	
	sylt_dprintf("- platform:\n");
	print_size(int);
	print_size(short);
	print_size(long);
	print_size(size_t);
	print_size(ptrdiff_t);
	print_size(float);
	print_size(double);
	
	sylt_dprintf("- sylt:\n");
	print_size(value_t);
	print_size(list_t);
	print_size(string_t);
	print_size(func_t);
	print_size(closure_t);
	print_size(upvalue_t);
	print_size(sylt_t);
	#undef print_size
	#endif
}

void dbg_print_header(
	const vm_t* vm, const closure_t* cls)
{
	const func_t* func = cls->func;
	
	sylt_dprintf("\n-> %.*s\n",
		(int)func->name->len,
		func->name->bytes);
	
	sylt_dprintf("depth %d/%d, ",
		(int)vm->nframes, MAX_CFRAMES);
	
	size_t used = vm->sp - vm->stack;
	sylt_dprintf("stack %ld/%ld\n",
		used, vm->maxstack);
	
	sylt_dprintf(
		"%ld bytes, "
		"%ld constants, "
		"%ld slots, "
		"%d params, "
		"%ld upvals\n",
		func->ncode,
		func->ndata,
		func->slots,
		func->params,
		cls->nupvals);
	
	#if DBG_PRINT_DATA
	sylt_dprintf("  constants:");
	for (size_t i = 0; i < func->ndata; i++)
	{
		string_t* str = val_tostring_opts(
			func->data[i],
			true,
			24,
			vm->ctx);
		
		sylt_dprintf(
			"    #%-2ld -- %.*s\n",
			i,
			(int)str->len,
			str->start);
	}
	#endif
		
	sylt_dprintf(
		"  addr  line opcode             "
		"hex\n");
	sylt_dprintf("  ");
	for (int i = 0; i < 40; i++)
		sylt_dprintf("-");
	sylt_dprintf("\n");
}

void dbg_print_instruction(
	const vm_t* vm, const func_t* func)
{
	op_t op = vm->fp->ip[-1];
	size_t addr =
		(size_t)
			(vm->fp->ip - 1 - func->code);
	sylt_dprintf("  %05ld", addr);
	
	/* TODO: max line limit */
	const uint32_t* lines =
		vm->fp->func->lines;
	uint32_t line = lines[addr];
	
	/* TODO: fix */
	if (addr == 0 || line > lines[addr - 1])
		sylt_dprintf(" %-4d ", line);
	else
		sylt_dprintf(" |    ");
		
	const char* name = OPINFO[op].name;
	sylt_dprintf("%-19s", name);
		
	/* hex values */
	int rank = OPINFO[op].rank;
	for (int i = -1; i < rank; i++) {
		uint8_t arg = vm->fp->ip[i];
		sylt_dprintf("%02x ", arg);
	}
		
	sylt_dprintf("\n");
}

void dbg_print_stack(
	const vm_t* vm, const func_t* func)
{
	const int maxvals = 5;
	
	/* don't print first iteration */
	if (vm->fp->ip - 1 == func->code)
		return;
	
	value_t* start =
		vm->stack + vm->fp->offs;
	
	/* only display the last N values */
	ptrdiff_t diff = vm->sp - start;
	if (diff > maxvals)
		start += diff - maxvals;
	
	value_t* v = start;
	
	if (diff > maxvals)
		sylt_dprintf("  [ <+%ld, ",
			diff - maxvals);
	else
		sylt_dprintf("  [ ");
	
	for (; v != vm->sp; v++) {
		val_print(*v, true, 12, vm->ctx);
		if (v != vm->sp - 1)
		  sylt_dprintf(", ");
	}
	
	sylt_dprintf(" ]\n");
}

/* pushes a value on the stack */
static inline void vm_push(
	vm_t* vm, value_t val)
{
	#if DBG_ASSERTIONS
	size_t used = vm->sp - vm->stack;
	assert(used <= vm->maxstack);
	#endif
	
	*vm->sp++ = val;
}

/* pops a value from the stack */
static inline value_t vm_pop(vm_t* vm) {
	assert(vm->sp != vm->stack);
	return *(--vm->sp);
}

/* shrinks the stack by n values */
static inline void vm_shrink(
	vm_t* vm, int n)
{
	assert(vm->sp - n >= vm->stack);
	vm->sp -= n;
}

/* returns the value n down the stack */
static inline value_t vm_peek(
	const vm_t* vm, int n)
{
	value_t* ptr = vm->sp + (-1 - n);
	
	#if DBG_ASSERTIONS
	bool out_of_bounds = ptr < vm->stack
		|| ptr > vm->sp;
	
	if (out_of_bounds) {
		sylt_dprintf(
			"access: %p\n"
			"base: %p\n"
			"top: %p\n",
			ptr,
			vm->stack,
			vm->sp);
		dbgerr("value stack overflowed");
	}
	#endif
	
	return *ptr;
}

/* virtual machine macros */
#define read() *vm->fp->ip++
#define read16() \
	(vm->fp->ip += 2, (uint16_t) \
		((vm->fp->ip[-2] << 8) \
			| vm->fp->ip[-1]))
#define push(v) vm_push(vm, (v))
#define pop() vm_pop(vm)
#define shrink(n) vm_shrink(vm, (n))
#define peek(n) vm_peek(vm, (n))

void vm_math(vm_t* vm, op_t opcode);

/* grows the stack if necessary */
void vm_ensurestack(vm_t* vm, int needed) {
	if (vm->maxstack >= needed)
		return;
	gc_pause(vm->ctx);
	size_t offset = vm->sp - vm->stack;
	
	vm->stack = arr_resize(
		vm->stack,
		value_t,
		vm->maxstack,
		needed,
		vm->ctx);	
	vm->maxstack = needed;
	gc_resume(vm->ctx);
	vm->sp = vm->stack + offset;
}

/* returns the value referenced by
 * an upvalue */
value_t vm_getupval(
	vm_t* vm, upvalue_t* upval)
{
	if (upval->index == -1)
		return upval->closed;
	return vm->stack[upval->index];
}

upvalue_t* vm_capupval(
	vm_t* vm, size_t index)
{
	upvalue_t* prev = NULL;
	upvalue_t* cur = vm->openups;
	while (cur && cur->index > index) {
		prev = cur;
		cur = cur->next;
	}
	
	if (cur && cur->index == index)
		return cur;
	
	upvalue_t* fresh = upvalue_new(
		vm->ctx, index);
	
	fresh->next = cur;
	if (!prev)
		vm->openups = fresh;
	else
		prev->next = fresh;
	
	return fresh;
}

void vm_closeupvals(
	vm_t* vm, size_t last)
{
	if (!vm->openups ||
		vm->openups->index < last)
		return;
	
	upvalue_t* upval = vm->openups;
	upval->closed = vm->stack[upval->index];
	upval->index = -1;
	vm->openups = upval->next;
}

void vm_exec(vm_t* vm) {
	sylt_set_state(vm->ctx, SYLT_STATE_EXEC);
	
	assert(peek(0).tag == TYPE_FUNCTION);
	const func_t* entry = getfunc(peek(0));
	assert(entry);
	
	/* make sure there's enough stack space */
	vm_ensurestack(vm, entry->slots);
	
	/* setup first call frame */
	cframe_t frame;
	frame.func = entry;
	frame.cls = closure_new(vm->ctx, entry);
	frame.ip = entry->code;
	frame.offs = 0;
	vm->frames[vm->nframes++] = frame;
	vm->fp = &vm->frames[vm->nframes - 1];
	
	/* the function is now reachable from
	 * the callstack and doesn't need to
	 * be on the stack */
	pop();
	
	/* ensure an empty stack */
	vm->sp = vm->stack;
	
	#if DBG_PRINT_CODE
	dbg_print_header(vm, vm->fp->cls);
	#endif
	
	for (;;) {
		uint8_t op = read();
		
		#if DBG_PRINT_STACK
		dbg_print_stack(vm,
			vm->fp->func);
		#endif
		
		#if DBG_PRINT_CODE
		dbg_print_instruction(vm,
			vm->fp->func);
		#endif
		
		switch (op) {
		/* == stack == */
		case OP_PUSH: {
			value_t val = vm->fp->func->
				data[read()];
			push(val);
			break;
		}
		case OP_PUSH_NIL: {
			push(wrapnil());
			break;
		}
		case OP_PUSH_TRUE: {
			push(wrapbool(true));
			break;
		}
		case OP_PUSH_FALSE: {
			push(wrapbool(false));
			break;
		}
		case OP_PUSH_LIST: {
			uint8_t len = read();
			list_t* ls = list_new(vm->ctx);
			
			push(wraplist(ls)); /* GC */
			for (int i = len; i > 0; i--) {
				list_push(
					ls, peek(i), vm->ctx);
			}
			pop(); /* GC */
			
			shrink(len);
			push(wraplist(ls));
			break;
		}
		case OP_PUSH_FUNC: {
			func_t* func = 
				getfunc(vm->fp->func->
					data[read()]);
			
			closure_t* cls =
				closure_new(vm->ctx, func);
			push(wrapclosure(cls));
			
			arr_alloc(
				cls->upvals,
				upvalue_t*,
				func->upvalues,
				vm->ctx);
			cls->nupvals = func->upvalues;
			
			for (size_t i = 0;
				i < cls->nupvals; i++)
			{
				uint8_t islocal = read();
				uint8_t index = read();
				if (islocal) {
					size_t pos =
						vm->fp->offs + index;
			
					cls->upvals[i] =
						vm_capupval(vm, pos);
				} else {
					cls->upvals[i] = vm->fp->
						cls->upvals[index];
				}
			}
			
			break;
		}
		case OP_POP: {
			pop();
			break;
		}
		case OP_DUP: {
			value_t val = peek(0);
			push(val);
			break;
		}
		case OP_SWAP: {
			value_t tmp = vm->sp[-2];
			vm->sp[-2] = vm->sp[-1];
			vm->sp[-1] = tmp;
			break;
		}
		case OP_LOAD: {
			value_t val =
				*(vm->stack
					+ vm->fp->offs
					+ read());
			push(val);
			break;
		}
		case OP_STORE: {
			value_t val = peek(0);
			*(vm->stack
				+ vm->fp->offs
				+ read()) = val;
			break;
		}
		case OP_LOAD_LIST: {
			value_t index = pop();
			typecheck(
				vm->ctx, index, TYPE_NUM);
				
			value_t list = pop();
			typecheck(
				vm->ctx, list, TYPE_LIST);
			
			push(list_get(
				getlist(list),
				getnum(index),
				vm->ctx));
			break;
		}
		case OP_LOAD_UPVAL: {
			uint8_t index = read();
			value_t val = vm_getupval(vm,
				vm->fp->cls->upvals[index]);
			push(val);
			break;
		}
		case OP_MOVE_HEAP: {
			size_t pos = 
				vm->sp - vm->stack - 1;
			vm_closeupvals(vm, pos);
			pop();
			break;
		}
		case OP_HIDE_RET: {
			vm->hidden = pop();
			break;
		}
		case OP_SHOW_RET: {
			push(vm->hidden);
			break;
		}
		/* == arithmetic == */
		case OP_ADD:
		case OP_SUB:
		case OP_MUL:
		case OP_DIV:
		case OP_EDIV: {
			vm_math(vm, op);
			break;
		}
		case OP_UMIN: {
			typecheck(
				vm->ctx,
				peek(0),
				TYPE_NUM);
			push(wrapnum(-getnum(pop())));
			break;
		}
		/* == comparison == */
		case OP_LT:
		case OP_LTE:
		case OP_GT:
		case OP_GTE: {
			vm_math(vm, op);
			break;
		}
		/* equality */
		case OP_EQ: {
			value_t b = pop();
			value_t a = pop();
			push(wrapbool(val_eq(a, b)));
			break;
		}
		case OP_NEQ: {
			value_t b = pop();
			value_t a = pop();
			push(wrapbool(!val_eq(a, b)));
			break;
		}
		case OP_NOT: {
			typecheck(
				vm->ctx,
				vm->sp[-1],
				TYPE_BOOL);
			push(wrapbool(!getbool(pop())));
			break;
		}
		/* == control flow == */
		case OP_JMP: {
			uint16_t offset = read16();
			vm->fp->ip += offset;
			break;
		}
		case OP_JMPIF: {
			uint16_t offset = read16();
			typecheck(
				vm->ctx, peek(0), TYPE_BOOL);
			if (getbool(peek(0)))
				vm->fp->ip += offset;
			break;
		}
		case OP_JMPIFN: {
			uint16_t offset = read16();
			typecheck(
				vm->ctx, peek(0), TYPE_BOOL);
			if (!getbool(peek(0)))
				vm->fp->ip += offset;
			break;
		}
		case OP_CALL: {
			uint8_t argc = read();
			typecheck(
				vm->ctx,
				peek(argc),
				TYPE_CLOSURE);
			
			closure_t* cls =
				getclosure(peek(argc));
			const func_t* func = cls->func;
				
			if (func->params != argc) {
				halt(vm->ctx, E_WRONGARGC(
					func->params, argc));
				unreachable();
			}
			
			if (func->cfunc) {
				/* calling a C function
				 * does not involve the
				 * sylt callstack */
				value_t result =
					func->cfunc(vm->ctx);
				shrink(argc + 1);
				push(result);
				
				#if DBG_PRINT_CODE
				sylt_dprintf("\n");
				#endif
				
				break;
			}
			
			if (vm->nframes == MAX_CFRAMES) {
				halt(vm->ctx,
					E_STACKOVERFLOW,
					MAX_CFRAMES);
				unreachable();
			}
			
			/* see if we have enough 
			 * stack space and grow if not */
			size_t used = vm->sp - vm->stack;
			size_t needed = used
				+ func->slots;
			vm_ensurestack(vm, needed);
			
			/* push a new call frame */
			cframe_t frame;
			frame.func = func;
			frame.cls = cls;
			frame.ip = func->code;
			frame.offs = used - argc;
			vm->frames[vm->nframes++] = frame;
			vm->fp = &vm->frames[
				vm->nframes - 1];
				
			#if DBG_PRINT_CODE
			dbg_print_header(vm,
				vm->fp->cls);
			#endif
			
			break;
		}
		case OP_RET: {
			/* save return value */
			value_t result = pop();
			
			/* move any values referenced by
			 * upvalues that are about to
			 * go off the stack to the heap */
			vm_closeupvals(vm, vm->fp->offs);
			
			vm->nframes--;
			if (vm->nframes == 0) {
				sylt_set_state(vm->ctx,
					SYLT_STATE_DONE);
				return;
			}
			
			/* shrink stack to get rid of
			 * function and arguments */
			vm->sp =
				vm->stack + vm->fp->offs - 1;
				
			/* restore return value */
			push(result);
			
			vm->fp =
				&vm->frames[vm->nframes - 1];
			
			#if DBG_PRINT_CODE
			dbg_print_header(
				vm, vm->fp->cls);
			#endif
			break;
		}
		default: unreachable(); return;
		}
	}
	
	unreachable();
}

void vm_math(vm_t* vm, op_t opcode) {
	typecheck(vm->ctx, peek(0), TYPE_NUM);
	sylt_num_t b = getnum(pop());
	
	typecheck(vm->ctx, peek(0), TYPE_NUM);
	sylt_num_t a = getnum(pop());
	
	switch (opcode) {
	/* arithmetic */
	case OP_ADD:
		push(wrapnum(a + b));
		break;
	case OP_SUB:
		push(wrapnum(a - b));
		break;
	case OP_MUL:
		push(wrapnum(a * b));
		break;
	case OP_DIV:
		if (b == 0.0f) {
			halt(vm->ctx, E_DIVBYZERO);
			unreachable();
		}
		
		push(wrapnum(a / b));
		break;
	case OP_EDIV: {
		if (b == 0.0f) {
			halt(vm->ctx, E_DIVBYZERO);
			unreachable();
		}
		
		sylt_num_t res =
			num_func(fmodf, fmod)(a, b);
		
		if (res < 0)
			res += fabs(b);
		push(wrapnum(res));
		break;
	}
	/* comparison */
	case OP_LT:
		push(wrapbool(a < b));
		break;
	case OP_LTE:
		push(wrapbool(a <= b));
		break;
	case OP_GT:
		push(wrapbool(a > b));
		break;
	case OP_GTE:
		push(wrapbool(a >= b));
		break;
	default: unreachable();
	}
}

#undef read
#undef read16
#undef push
#undef pop
#undef peek

/* == standard library == */

#define argc() (ctx->vm->fp->ip[-1])
#define arg(n) \
	vm_peek(ctx->vm, argc() - (n) - 1)

#define boolarg(n) getbool(arg(n))
#define numarg(n) getnum(arg(n))
#define listarg(n) getlist(arg(n))
#define stringarg(n) getstring(arg(n))

/* == prelude lib == */

/* prints arg(0) to the standard output
 * and flushes the stream  */
value_t std_put(sylt_t* ctx) {
	val_print(arg(0), false, 0, ctx);
	fflush(stdout);
	return wrapnil();
}

/* prints arg(0) to the standard output,
 * followed by a newline (\n) character
 * (which flushes the stream automatically) */
value_t std_putln(sylt_t* ctx) {
	val_print(arg(0), false, 0, ctx);
	sylt_printf("\n");
	return wrapnil();
}

/* returns arg(0) converted to a string */
value_t std_tostring(sylt_t* ctx) {
	string_t* str =
		val_tostring(arg(0), ctx);
	return wrapstring(str);
}

/* returns the type of arg(0) as a string */
value_t std_typeof(sylt_t* ctx) {
	return wrapstring(string_lit(
		user_type_name(arg(0).tag), ctx));
}

/* meant for debug builds, halts the VM with
 * an error if arg(0) evaluates to false */
value_t std_ensure(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_BOOL);
	if (!boolarg(0)) {
		halt(ctx, "ensure failed");
		unreachable();
	}
	
	return arg(0);
}

/* == list lib == */

/* returns the length of arg(0) */
value_t std_length(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_LIST);
	return wrapnum(listarg(0)->len);
}

/* appends a value to the end of arg(0) */
value_t std_push(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_LIST);
	list_push(listarg(0), arg(1), ctx);
	return wrapnil();
}

/* removes and returns the last value of
 * arg(0), or nil if it was empty */
value_t std_pop(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_LIST);
	return list_pop(listarg(0), ctx);
}

/* == math lib == */

/* returns true if arg(0) is nearly equal
 * to arg(1), within a tolerance of arg(2) */
value_t std_nearly(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_NUM);
	
	sylt_num_t a = numarg(0);
	sylt_num_t b = numarg(1);
	if (a == b)
		return wrapbool(true);
	
	sylt_num_t epsilon = getnum(arg(2));
	sylt_num_t diff =
		num_func(fabsf, fabs)(a - b);
	
	return wrapbool(diff < epsilon);
}

/* returns the absolute value of arg(0) */
value_t std_abs(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	sylt_num_t result =
		num_func(fabsf, fabs)(numarg(0));
	return wrapnum(result);
}

/* returns the exponent to which the base
 * arg(1) needs to be raised in order to 
 * produce the target value arg(0) */
value_t std_log(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_NUM);
	
	sylt_num_t x = numarg(0);
	sylt_num_t base = numarg(1);
	sylt_num_t result = 1;
	
	/* optimize for common bases */
	if (base == 2) {
		/* binary */
		result = num_func(log2f, log2)(x);
	
	} else if (base == M_E) {
		/* natural */
		result = num_func(logf, log)(x);
	
	} else if (base == 10) {
		/* common */
		result = num_func(log10f, log10)(x);
	
	} else {
		result = num_func(logf, log)(x)
			/ num_func(logf, log)(base);
	}
	
	return wrapnum(result);
}

/* returns arg(0) raised to the power of
 * arg(1) */
value_t std_raise(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_NUM);
	
	sylt_num_t a = numarg(0);
	sylt_num_t b = numarg(1);
	sylt_num_t result =
		num_func(powf, pow)(a, b);
	return wrapnum(result);
}

/* returns the square root of arg(0) */
value_t std_sqrt(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	sylt_num_t result =
		num_func(sqrtf, sqrt)(numarg(0));
	return wrapnum(result);
}

/* returns arg(0) rounded towards -infinity */
value_t std_floor(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(floorf, floor)(numarg(0)));
}

/* returns arg(0) rounded towards +infinity */
value_t std_ceil(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(ceilf, ceil)(numarg(0)));
}

/* returns the nearest value to arg(0), 
 * rounding halfway cases away from zero */
value_t std_round(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(roundf, round)(numarg(0)));
}

/* returns arg(0) converted from degrees to
 * radians */
value_t std_rads(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(numarg(0) * M_PI / 180.0);
}

/* returns arg(0) converted from radians to
 * degrees */
value_t std_degs(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(numarg(0) * 180.0 / M_PI);
}

/* returns the sine of arg(0) */
value_t std_sin(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(sinf, sin)(numarg(0)));
}

/* returns the cosine of arg(0) */
value_t std_cos(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(cosf, cos)(numarg(0)));
}

/* returns the tangent of arg(0) */
value_t std_tan(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(tanf, tan)(numarg(0)));
}

void std_add(sylt_t* ctx,
	const char* name,
	value_t val);

void std_addf(
	sylt_t* ctx,
	const char* name,
	cfunc_t cfunc,
	int params);

void load_stdlib(sylt_t* ctx) {
	/* prelude */
	std_addf(ctx, "put", std_put, 1);
	std_addf(ctx, "putln", std_putln, 1);
	std_addf(ctx, "tostring",
		std_tostring, 1);
	std_addf(ctx, "typeof", std_typeof, 1);
	std_addf(ctx, "ensure", std_ensure, 1);
	
	/* list */
	std_addf(ctx, "length", std_length, 1);
	std_addf(ctx, "push", std_push, 2);
	std_addf(ctx, "pop", std_pop, 1);
	
	/* math */
	std_add(ctx, "PI", wrapnum(M_PI));
	std_add(ctx, "E", wrapnum(M_E));
	std_addf(ctx, "nearly", std_nearly, 3);
	std_addf(ctx, "abs", std_abs, 1);
	std_addf(ctx, "log", std_log, 2);
	std_addf(ctx, "raise", std_raise, 2);
	std_addf(ctx, "sqrt", std_sqrt, 1);
	std_addf(ctx, "floor", std_floor, 1);
	std_addf(ctx, "ceil", std_ceil, 1);
	std_addf(ctx, "round", std_round, 1);
	std_addf(ctx, "rads", std_rads, 1);
	std_addf(ctx, "degs", std_degs, 1);
	std_addf(ctx, "sin", std_sin, 1);
	std_addf(ctx, "cos", std_cos, 1);
	std_addf(ctx, "tan", std_tan, 1);
}

/* == compiler == */

typedef enum {
	T_NAME,
	T_NIL,
	T_TRUE,
	T_FALSE,
	T_LET,
	T_FUN,
	T_IF,
	T_ELSE,
	T_MATCH,
	T_WITH,
	T_AND,
	T_OR,
	T_STRING,
	T_NUMBER,
	T_PLUS,
	T_MINUS,
	T_MINUS_GREATER,
	T_STAR,
	T_SLASH,
	T_PERCENT,
	T_LESS,
	T_LESS_EQ,
	T_GREATER,
	T_GREATER_EQ,
	T_EQ,
	T_EQ_EQ,
	T_BANG,
	T_BANG_EQ,
	T_LPAREN,
	T_RPAREN,
	T_LCURLY,
	T_RCURLY,
	T_LSQUARE,
	T_RSQUARE,
	T_PIPE,
	T_QUESTION,
	T_COMMA,
	T_EOF,
} token_type_t;

/* the source code is scanned into a series of
 * tokens */
typedef struct {
	/* token type */
	token_type_t tag;
	/* pointer to the start of the token */
	const char* start;
	/* length of the token */
	size_t len;
	/* line number */
	uint32_t line;
} token_t;

/* operator precedence levels,
 * from lowest to highest */
typedef enum {
	PREC_NONE,
	/* assignment '=' */
	PREC_ASSIGN,
	/* boolean 'or' */
	PREC_OR,
	/* boolean 'and' */
	PREC_AND,
	/* equality: == != */
	PREC_EQ,
	/* comparison: < <= > >= */
	PREC_CMP,
	/* addition: + - */
	PREC_TERM,
	/* multiplication: * / % */
	PREC_FACTOR,
	/* unary prefix: - ! */
	PREC_UPRE,
	/* unary postfix: () [] */
	PREC_UPOST,
	PREC_PRIMARY,
} prec_t;

const int ANY_PREC = PREC_ASSIGN;

typedef struct {
	token_t name;
	/* scope depth */
	int depth;
	/* captured by a closure */
	bool capped;
} symbol_t;

typedef struct {
	bool islocal;
	uint8_t index;
} cmp_upvalue_t;

/* compiler + lexer & parser state */
typedef struct comp_s {
	/* for nested function declarations */
	struct comp_s* parent;
	struct comp_s* child;
	/* output */
	func_t* func;
	/* current (simulated) stack size */
	int curslots;
	
	/* source code */
	string_t* src;
	/* scanner position */
	char* pos;
	/* scanner line */
	uint32_t line;
	/* parsing tokens */
	token_t prev;
	token_t cur;
	/* symbol table */
	symbol_t* names;
	size_t nnames;
	/* current scope depth */
	int depth;
	/* upvalues for current function */
	cmp_upvalue_t upvals[MAX_UPVALUES];
	/* reference to API */
	sylt_t* ctx;
} comp_t;

void comp_init(
	comp_t* cmp,
	comp_t* parent,
	comp_t* child,
	sylt_t* ctx)
{
	string_t* name = sylt_peekstring(ctx, 0);
	string_t* src = sylt_peekstring(ctx, 1);
	
	cmp->parent = parent;
	cmp->child = child;
	cmp->func = func_new(ctx, name);
	cmp->curslots = 0;
	
	cmp->src = src;
	cmp->pos = (char*)cmp->src->bytes;
	cmp->line = 1;
	cmp->names = NULL;
	cmp->nnames = 0;
	cmp->depth = 0;
	cmp->ctx = ctx;
	
	/* the source code and name are now
	 * reachable from the compiler and
	 * can be safely taken off the stack */
	sylt_shrink(ctx, 2);
}

void comp_free(comp_t* cmp) {
	arr_free(
		cmp->names,
		symbol_t,
		cmp->nnames,
		cmp->ctx);
}

/* == lexer == */

/* scans the source code for the next token */
token_t scan(comp_t* cmp) {
	#define token(tag) \
		(token_t){ \
			tag, \
			start, \
			cmp->pos - start, \
			cmp->line}
	#define step() cmp->pos++
	#define peek() (*cmp->pos)
	#define peek2() \
		(eof() ? '\0' : cmp->pos[1])
	#define is(c) (peek() == (c))
	#define next_is(c) (peek2() == (c))
	#define eof() is('\0')
	#define match(c) \
		((!eof() && is(c)) ? \
			step(), true : false)
	
	/* skip any initial whitespace */
	for (;;) {
		if (!isspace(peek())) {
			/* single-line comment */
			if (is('#') && next_is('#')) {
				while (!is('\n') && !eof()) {
					step();
				}
				
			} else {
				break;
			}
		}
		
		if (is('\n'))
			cmp->line++;
		
		step();
	}
	
	/* remember first non-whitespace char */
	const char* start = cmp->pos;
	if (eof())
		return token(T_EOF);
	
	/* symbol name or keyword */
	if (isalpha(peek()) || is('_')) {
		while (isalnum(peek()) || is('_'))
			step();
		
		size_t len = cmp->pos - start;
		#define keyword(lit) \
			(len == strlen(lit) && !strncmp( \
				start, (lit), strlen(lit)))
		
		if (keyword("nil"))
			return token(T_NIL);
		if (keyword("true"))
			return token(T_TRUE);
		if (keyword("false"))
			return token(T_FALSE);
		if (keyword("let"))
			return token(T_LET);
		if (keyword("fun"))
			return token(T_FUN);
		if (keyword("if"))
			return token(T_IF);
		if (keyword("else"))
			return token(T_ELSE);
		if (keyword("match"))
			return token(T_MATCH);
		if (keyword("with"))
			return token(T_WITH);
		if (keyword("and"))
			return token(T_AND);
		if (keyword("or"))
			return token(T_OR);
			
		#undef keyword
		
		return token(T_NAME);
	}
	
	/* string literal */
	if (is('"')) {
		step();
		while (!is('"') && !eof()) {
			step();
			
			/* don't terminate the string if
			 * we find a '\"', unless the
			 * backslash is part of a '\\' */
			bool ignore_close = is('"')
				&& cmp->pos - start >= 2
				&& (cmp->pos[-1] == '\\'
					&& cmp->pos[-2] != '\\');
					
			if (ignore_close)
				step();
		}
		
		if (eof())
			halt(cmp->ctx, E_UNTERMSTRING);
		
		step();
		return token(T_STRING);
	}
	
	/* numbers begin with a digit */
	if (isdigit(peek())) {
		while (isdigit(peek()))
			step();
		
		if (is('.')) {
			step();
			while (isdigit(peek()))
				step();
		}
			
		return token(T_NUMBER);
	}
	
	/* see if we can find an operator */
	switch (*step()) {
	case '+': return token(T_PLUS);
	case '-':
		return token((!match('>'))
			? T_MINUS
			: T_MINUS_GREATER);
	case '*': return token(T_STAR);
	case '/': return token(T_SLASH);
	case '%': return token(T_PERCENT);
	case '<':
		return token((!match('='))
			? T_LESS
			: T_LESS_EQ);
	case '>':
		return token((!match('='))
			? T_GREATER
			: T_GREATER_EQ);
	case '=':
		return token((!match('='))
			? T_EQ
			: T_EQ_EQ);
	case '!':
		return token((!match('='))
			? T_BANG
			: T_BANG_EQ);
	case '(': return token(T_LPAREN);
	case ')': return token(T_RPAREN);
	case '{': return token(T_LCURLY);
	case '}': return token(T_RCURLY);
	case '[': return token(T_LSQUARE);
	case ']': return token(T_RSQUARE);
	case '|': return token(T_PIPE);
	case '?': return token(T_QUESTION);
	case ',': return token(T_COMMA);
	}
	
	cmp->prev.line = cmp->line; /* hack */
	halt(cmp->ctx,
		E_UNEXPECTEDCHAR(cmp->pos[-1]));
	return token(-1);
	
	#undef token
	#undef step
	#undef peek
	#undef peek2
	#undef is
	#undef next_is
	#undef eof
	#undef match
}

/* == codegen == */

void comp_simstack(comp_t* cmp, int n) {
	cmp->curslots += n;
	assert(cmp->curslots >= 0);
	
	if (cmp->curslots > MAX_STACK) {
		halt(cmp->ctx, E_STACKLIMIT);
		unreachable();
	}
	
	/* record the largest stack size */
	if (cmp->curslots > cmp->func->slots)
		cmp->func->slots = cmp->curslots;
}

/* should not be used directly; use the
 * emit_ functions below instead */
void emit_op(
	comp_t* cmp,
	op_t op,
	const uint8_t* args,
	size_t nargs)
{
	comp_simstack(cmp, OPINFO[op].effect);
	
	/* write the instruction opcode */
	func_write(
		cmp->func,
		op,
		cmp->prev.line,
		cmp->ctx);
	
	/* write the arguments */
	for (size_t i = 0; i < nargs; i++) {
		func_write(
			cmp->func,
			args[i],
			cmp->prev.line,
			cmp->ctx);
	}
}

/* emits an instruction with no operands */
void emit_nullary(comp_t* cmp, op_t op) {
	assert(OPINFO[op].rank == 0);
	emit_op(cmp, op, NULL, 0);
}

/* emits an instruction with one operand */
void emit_unary(
	comp_t* cmp, op_t op, uint8_t arg)
{
	assert(OPINFO[op].rank == 1);
	const uint8_t args[] = {arg};
	emit_op(cmp, op, args, 1);
}

/* emits an instruction with two operands */
void emit_binary(
	comp_t* cmp,
	op_t op,
	uint8_t a,
	uint8_t b)
{
	assert(OPINFO[op].rank == 2);
	const uint8_t args[] = {a, b};
	emit_op(cmp, op, args, 2);
}

/* emits an instruction to push a value
 * on to the operand stack */
void emit_value(
	comp_t* cmp, value_t val)
{
	sylt_push(cmp->ctx, val); /* GC */
	
	#if OPTZ_SPECIAL_PUSHOPS
	switch (val.tag) {
	case TYPE_NIL: {
		emit_nullary(cmp, OP_PUSH_NIL);
		sylt_pop(cmp->ctx); /* GC */
		return;
	}
	case TYPE_BOOL: {
		emit_nullary(cmp, (getbool(val))
			? OP_PUSH_TRUE
			: OP_PUSH_FALSE);
		sylt_pop(cmp->ctx); /* GC */
		return;
	}
	default: break;
	}
	#endif
	
	size_t slot = func_write_data(
		cmp->func, val, cmp->ctx);
		
	if (val.tag == TYPE_FUNCTION) {
		emit_unary(cmp, OP_PUSH_FUNC, slot);
	} else {
		emit_unary(cmp, OP_PUSH, slot);
	}
	
	sylt_pop(cmp->ctx); /* GC */
}

/* emits a jump instruction followed by
 * two placeholder bytes, returning an
 * address used for backpatching them
 * into a 16-bit jump offset later on */
int emit_jump(comp_t* cmp, op_t opcode) {
	emit_binary(cmp, opcode, 0xff, 0xff);
	return cmp->func->ncode - 2;
}

/* takes the return value of emit_jump
 * after we've emitted the code we need to
 * jump over and then backpatches the
 * jump target short to the correct offset */
void patch_jump(comp_t* cmp, int addr) {
	int dist =
		cmp->func->ncode - addr - 2;
	if (dist > MAX_JUMP) {
		halt(cmp->ctx, E_JUMPLIMIT);
		return;
	}
	
	/* encode short as two bytes */
	cmp->func->code[addr] =
		(dist >> 8) & 0xff;
	cmp->func->code[addr + 1] =
		dist & 0xff;
}

/* adds a name to the symbol table
 * and returns its index */
int add_symbol(comp_t* cmp, token_t name) {
	#if DBG_PRINT_NAMES
	for (int i = 0; i < cmp->depth; i++)
		sylt_dprintf("  ");
	sylt_dprintf("(sym) %.*s\n",
		(int)name.len, name.start);
	#endif
	
	symbol_t sym;
	sym.name = name;
	sym.depth = cmp->depth;
	sym.capped = false;
	
	cmp->names = arr_resize(
		cmp->names,
		symbol_t,
		cmp->nnames,
		cmp->nnames + 1,
		cmp->ctx);
	
	cmp->names[cmp->nnames++] = sym;
	return cmp->nnames - 1;
}

/* returns the index of a local variable
 * in the symbol table or -1 if not found */
int find_symbol(comp_t* cmp, token_t name) {
	if (cmp->nnames == 0)
		return -1;
	
	/* search backwards in case of
	 * shadowed variable names */
	int start = cmp->nnames - 1;
	for (int i = start; i >= 0; i--) {
		token_t other = cmp->names[i].name;
		bool match =
			other.len == name.len
			&& memcmp(
				name.start,
				other.start,
				name.len) == 0;
		
		if (match)
			return i;
	}
	
	return -1;
}

/* adds an upvalue to the upvalue array
 * and returns its index */
int add_upvalue(
	comp_t* cmp, uint8_t index, bool islocal)
{
	size_t n = cmp->func->upvalues;
	
	/* check if one already exists */
	for (size_t i = 0; i < n; i++) {
		cmp_upvalue_t* upval =
			&cmp->upvals[i];
		if (upval->index == index
			&& upval->islocal == islocal)
		{
			return i;
		}
	}
	
	if (n == MAX_UPVALUES) {
		halt(cmp->ctx, E_TOOMANYUPVALUES);
		unreachable();
	}
	
	cmp->upvals[n].islocal = islocal;
	cmp->upvals[n].index = index;
	return cmp->func->upvalues++;
}

/* returns the index of an upvalue in the
 * upvalue array or -1 if not found */
int find_upvalue(comp_t* cmp, token_t name) {
	if (!cmp->parent)
		return -1;
	
	/* search for a local variable in the
	 * enclosing function */
	int local =
		find_symbol(cmp->parent, name);
	if (local != -1) {
		cmp->parent->names[local].capped =
			true;
		return add_upvalue(cmp, local, true);
	}
	
	/* recursively search for an upvalue
	 * higher up */
	int upvalue =
		find_upvalue(cmp->parent, name);
	if (upvalue != -1) {
		return
			add_upvalue(cmp, upvalue, false);
	}
	
	return -1;
}

/* == parser == */

/* steps the parser one token forward */
void step(comp_t* cmp) {
	cmp->prev = cmp->cur;
	
	#if DBG_PRINT_TOKENS
	sylt_dprintf("%-4d '%.*s'\n",
		cmp->prev.tag,
		cmp->prev.len,
		cmp->prev.start);
	#endif
	
	cmp->cur = scan(cmp);
}

/* returns true if the next token is of
 * the provided type */
bool check(comp_t* cmp, token_type_t tag) {
 	return cmp->cur.tag == tag;
}

/* consumes the next token if it has the
 * given tag, returning true on success */
bool match(comp_t* cmp, token_type_t tag) {
	if (check(cmp, tag))
		step(cmp);
	return cmp->prev.tag == tag;
}

/* consumes the next token if the tag matches,
 * throws an error if it does not */
void eat(
	comp_t* cmp,
	token_type_t tag,
	const char* msg)
{
	if (check(cmp, tag)) {
		step(cmp);
		return;
	}
	
	halt(cmp->ctx, msg);
}

/* parsing functions */
void expr(comp_t*, prec_t);
void literal(comp_t*);
void name(comp_t*);
void list(comp_t*);
void string(comp_t*);
void number(comp_t*);
void grouping(comp_t*);
void unary(comp_t*);
void binary(comp_t*);
void call(comp_t*);
void index(comp_t*);
void let(comp_t*);
void fun(comp_t*);
void if_else(comp_t*);
void match_with(comp_t*);
void block(comp_t*);

typedef void (*parsefn_t)(comp_t*);

void printfname(
	const char* name, sylt_t* ctx)
{
	#if DBG_PRINT_AST
	comp_t* cmp = ctx->cmp;
	while (cmp) {
		sylt_dprintf("  ");
		cmp = cmp->child;
	}
	
	sylt_dprintf("  %s\n", name);
	#endif
}

#define dbg_printfname() \
	printfname(__func__, cmp->ctx)

/* all tokens match a parse rule */
typedef struct {
	parsefn_t prefix;
	parsefn_t infix;
	prec_t prec;
} parserule_t;

/* maps tokens to parsers */
static parserule_t RULES[] = {
	[T_NAME] = {name, NULL, PREC_NONE},
	[T_NIL] = {literal, NULL, PREC_NONE},
	[T_TRUE] = {literal, NULL, PREC_NONE},
	[T_FALSE] = {literal, NULL, PREC_NONE},
	[T_LET] = {let, NULL, PREC_NONE},
	[T_FUN] = {fun, NULL, PREC_NONE},
	[T_IF] = {if_else, NULL, PREC_NONE},
	[T_ELSE] = {NULL, NULL, PREC_NONE},
	[T_MATCH] = {match_with, NULL, PREC_NONE},
	[T_WITH] = {NULL, NULL, PREC_NONE},
	[T_AND] = {NULL, binary, PREC_AND},
	[T_OR] = {NULL, binary, PREC_OR},
	[T_STRING] = {string, NULL, PREC_NONE},
	[T_NUMBER] = {number, NULL, PREC_NONE},
	[T_PLUS] = {NULL, binary, PREC_TERM},
	[T_MINUS] = {unary, binary, PREC_TERM},
	[T_STAR] = {NULL, binary, PREC_FACTOR},
	[T_SLASH] = {NULL, binary, PREC_FACTOR},
	[T_PERCENT] = {NULL, binary, PREC_FACTOR},
	[T_LESS] = {NULL, binary, PREC_CMP},
	[T_LESS_EQ] = {NULL, binary, PREC_CMP},
	[T_GREATER] = {NULL, binary, PREC_CMP},
	[T_GREATER_EQ] = {NULL, binary, PREC_CMP},
	[T_EQ] = {NULL, NULL, PREC_NONE},
	[T_EQ_EQ] = {NULL, binary, PREC_EQ},
	[T_BANG] = {unary, NULL, PREC_NONE},
	[T_BANG_EQ] = {NULL, binary, PREC_EQ},
	[T_LPAREN] = {grouping, call, PREC_UPOST},
	[T_RPAREN] = {NULL, NULL, PREC_NONE},
	[T_LCURLY] = {block, NULL, PREC_NONE},
	[T_RCURLY] = {NULL, NULL, PREC_NONE},
	[T_LSQUARE] = {list, index, PREC_UPOST},
	[T_RSQUARE] = {NULL, NULL, PREC_NONE},
	[T_PIPE] = {NULL, NULL, PREC_NONE},
	[T_QUESTION] = {NULL, NULL, PREC_NONE},
	[T_COMMA] = {NULL, NULL, PREC_NONE},
	[T_EOF] = {NULL, NULL, PREC_NONE},
};

/* parses an expression at or above
 * the given precedence level */
void expr(comp_t* cmp, prec_t prec) {
	/* move to the next token */
	step(cmp);
	
	parsefn_t prefix =
		RULES[cmp->prev.tag].prefix;
	
	/* first token in an expression
	 * must have a prefix rule */
	if (!prefix) {
		halt(cmp->ctx,
			"expected expression, got '%.*s'",
			(int)cmp->prev.len,
			cmp->prev.start);
		return;
	}
	
	prefix(cmp);
	
	/* parse the rest of the expression */
	while (prec <= RULES[cmp->cur.tag].prec) {
		step(cmp);
		parsefn_t infix =
			RULES[cmp->prev.tag].infix;
		infix(cmp);
	}
}

/* parses a keyword literal */
void literal(comp_t* cmp) {
	dbg_printfname();
	
	switch (cmp->prev.tag) {
	case T_NIL:
		emit_value(cmp, wrapnil());
		break;
	case T_TRUE:
		emit_value(cmp, wrapbool(true));
		break;
	case T_FALSE:
		emit_value(cmp, wrapbool(false));
		break;
	default: unreachable();
	}
}

/* parses a symbol name */
void name(comp_t* cmp) {
	dbg_printfname();
	
	int index = find_symbol(cmp, cmp->prev);
	if (index != -1) {
		emit_unary(cmp, OP_LOAD, index);
		return;
	}
	
	index = find_upvalue(cmp, cmp->prev);
	if (index != -1) {
		emit_unary(cmp, OP_LOAD_UPVAL, index);
		return;
	}
	
	halt(cmp->ctx, E_UNDEFINED(
		cmp->prev.len, cmp->prev.start));
}

/* parses a list literal */
void list(comp_t* cmp) {
	dbg_printfname();
	
	int len = 0;
	while (!check(cmp, T_RSQUARE)
		&& !check(cmp, T_EOF))
	{
		expr(cmp, PREC_OR);
		len++;
		
		if (!match(cmp, T_COMMA))
			break;
	}
	
	eat(cmp, T_RSQUARE,
		"unterminated list (expected ']')");
	emit_unary(cmp, OP_PUSH_LIST, len);
}

/* parses a string literal */
void string(comp_t* cmp) {
	dbg_printfname();
	
	token_t token = cmp->prev;
	assert(token.len >= 2); /* "" */
	
	/* allocate an empty string the same size
	 * as the length of the string literal */
	string_t* dst = string_new(
		NULL,
		token.len - 2,
		false,
		cmp->ctx);
	size_t write = 0;
	
	const char* src = token.start;
	size_t end = token.len - 1;
	
	for (size_t read = 1; read < end;) {
		if (src[read] != '\\') {
			/* append a regular character */
			dst->bytes[write++] =
				src[read++];
			continue;
		}
		
		char code = src[read + 1];
		int seqlen =
			(code == 'x') ? 4 : 2;
			
		switch (code) {
		case 'x': {
			/* make sure we have two digits */
			if (end - read < seqlen) {
				size_t needed =
					seqlen - (end - read);
			
				halt(cmp->ctx,
					"expected %ld more "
					"character%s after \\%c",
				needed,
				(needed > 1) ? "s" : "",
				code);
			}
			
			/* parse hexadecimal byte */
			char* start =
				(char*)src + read + 2;
			char* end = start + 2;
			unsigned long byte = strtoul(
				start, &end, 16);
			
			/* parsing failed */
			if (end < start + 2)
				halt(cmp->ctx,
					"invalid character in "
					"\\x escape: '%c'", *end);
				
			dst->bytes[write++] = byte;
			break;
		}
		case '\\':
			dst->bytes[write++] = '\\';
			break;
		case '\"':
			dst->bytes[write++] = '\"';
			break;
		case 't':
			dst->bytes[write++] = '\t';
			break;
		case 'n':
			dst->bytes[write++] = '\n';
			break;
		case 'r':
			dst->bytes[write++] = '\r';
			break;
		case '0':
			dst->bytes[write++] = '\0';
			break;
		default:
			halt(cmp->ctx,
				E_ESCAPESEQ(code));
		}
			
		read += seqlen;
	}
	
	/* shrink to fit */
	if (write < dst->len) {
		dst->bytes = arr_resize(
			dst->bytes,
			uint8_t,
			dst->len,
			write,
			cmp->ctx);
		dst->len = write;
	}
	
	emit_value(cmp, wrapstring(dst));
}

/* parses a numeric literal */
void number(comp_t* cmp) {
	dbg_printfname();
	
	sylt_num_t num = num_func(strtof, strtod)
		(cmp->prev.start, NULL);
	
	emit_value(cmp, wrapnum(num));
}

/* parses a parenthesized expression */
void grouping(comp_t* cmp) {
	dbg_printfname();
	
	expr(cmp, ANY_PREC);
	eat(cmp, T_RPAREN,
		"expected closing ')'");
}

/* parses a unary expression */
void unary(comp_t* cmp) {
	dbg_printfname();
	
	/* save the operator token */
	token_type_t token = cmp->prev.tag;
	
	/* parse the operand */
	expr(cmp, PREC_UPRE);
	
	op_t opcode;
	switch (token) {
	case T_MINUS: opcode = OP_UMIN; break;
	case T_BANG: opcode = OP_NOT; break;
	default: unreachable();
	}
	
	emit_nullary(cmp, opcode);
}

/* parses a binary expression */
void binary(comp_t* cmp) {
	dbg_printfname();
	/* left hand expression has already been
	 * compiled */
	
	/* save the operator token */
	token_type_t token = cmp->prev.tag;
	
	/* compile the right hand expression */
	parserule_t* rule = &RULES[token];
	
	op_t opcode;
	switch (token) {
	/* arithmetic */
	case T_PLUS: opcode = OP_ADD; break;
	case T_MINUS: opcode = OP_SUB; break;
	case T_STAR: opcode = OP_MUL; break;
	case T_SLASH: opcode = OP_DIV; break;
	case T_PERCENT: opcode = OP_EDIV; break;
	/* comparison */
	case T_LESS: opcode = OP_LT; break;
	case T_LESS_EQ: opcode = OP_LTE; break;
	case T_GREATER: opcode = OP_GT; break;
	case T_GREATER_EQ: opcode = OP_GTE; break;
	/* equality */
	case T_EQ_EQ: opcode = OP_EQ; break;
	case T_BANG_EQ: opcode = OP_NEQ; break;
	/* control flow */
	case T_AND: {
		/* if the left-hand side expression
		 * is false we jump past the 
		 * right-hand side expression */
		int jump = emit_jump(cmp, OP_JMPIFN);
		
		emit_nullary(cmp, OP_POP);
		expr(cmp, PREC_AND);
		
		patch_jump(cmp, jump);
		return; /* early return */
	}
	case T_OR: {
		int jump = emit_jump(cmp, OP_JMPIF);
		
		emit_nullary(cmp, OP_POP);
		expr(cmp, PREC_OR);
		
		patch_jump(cmp, jump);
		return; /* early return */
	}
	default: unreachable();
	}
	
	expr(cmp, rule->prec + 1);
	emit_nullary(cmp, opcode);
}

/* parses a function call operator */
void call(comp_t* cmp) {
	dbg_printfname();
	
	int argc = 0;
	
	/* parse argument list */
	while (!check(cmp, T_RPAREN)
		&& !check(cmp, T_EOF))
	{
		if (argc >= MAX_PARAMS) {
			halt(cmp->ctx, E_TOOMANYARGS);
			unreachable();
		}
			
		expr(cmp, ANY_PREC);
		argc++;
			
		if (!match(cmp, T_COMMA))
			break;
	}
	
	eat(cmp, T_RPAREN, "expected ')'");
	emit_unary(cmp, OP_CALL, argc);
	comp_simstack(cmp, -argc);
}

/* parses a subscript operator */
void index(comp_t* cmp) {
	dbg_printfname();
	
	expr(cmp, PREC_OR);
	eat(cmp, T_RSQUARE, "expected ']'");
	emit_nullary(cmp, OP_LOAD_LIST);
}

void parse_func(
	comp_t* cmp, token_t name, bool);

/* parses a variable or function binding */
void let(comp_t* cmp) {
	dbg_printfname();
	
	eat(cmp, T_NAME,
		"expected variable name after 'let'");
	
	/* remember the name token */
	token_t name = cmp->prev;
	
	if (match(cmp, T_LPAREN)) {
		 /* parse a function declaration
 		 * in the form of
 		 * let name(p1, p2, ..) = body */
 
		 /* add symbol first in order
	 	 * to support recursion */
		 add_symbol(cmp, name);
		 
		 parse_func(cmp, name, false);
		
	} else {
		/* parse a variable declaration 
		 * in the form of 
		 * let name = expr */
		eat(cmp, T_EQ,
			"expected '=' after "
			"variable name");
		
		/* compile the right hand side
	 	* of the expression */
		expr(cmp, ANY_PREC);
		add_symbol(cmp, name);
	}
	
	/* expression yields a 'nil' */
	emit_value(cmp, wrapnil());
}

/* parses an anonymous function */
void fun(comp_t* cmp) {
	dbg_printfname();
	
	eat(cmp, T_LPAREN,
		"expected '(' after 'fun' keyword");
	
	token_t name =
		(token_t){T_NAME, "_", 1, cmp->line};
	parse_func(cmp, name, true);
}

void comp_copystate(
	comp_t* dst, const comp_t* src)
{
	dst->pos = src->pos;
	dst->line = src->line;
	dst->prev = src->prev;
	dst->cur = src->cur;
}

void parse_func(
	comp_t* cmp,
	token_t name_tok,
	bool lambda)
{
	dbg_printfname();
	string_t* name = string_new(
		name_tok.start,
		name_tok.len,
		false, 
		cmp->ctx);
		
	sylt_pushstring(cmp->ctx, cmp->src);
	sylt_pushstring(cmp->ctx, name);
	
	/* setup a new compiler instance
	* in order to parse the function */
	comp_t fcmp;
	comp_init(&fcmp, cmp, NULL, cmp->ctx);
	comp_copystate(&fcmp, cmp);
	cmp->child = &fcmp;
		
	/* parse parameter list */
	while (!check(&fcmp, T_RPAREN)
		&& !check(&fcmp, T_EOF))
	{
		if (fcmp.func->params >= MAX_PARAMS) {
			halt(fcmp.ctx, E_TOOMANYPARAMS);
			unreachable();
		}
			
		eat(&fcmp, T_NAME,
			"expected parameter name");
			
		token_t name = fcmp.prev;
		add_symbol(&fcmp, name);
		fcmp.func->params++;
			
		if (!match(&fcmp, T_COMMA))
			break;
	}
		
	eat(&fcmp, T_RPAREN,
		"expected ')' or a parameter name");
	
	if (lambda)
		eat(&fcmp, T_MINUS_GREATER,
			"expected '->' after ')'");
	else
		eat(&fcmp, T_EQ,
			"expected '=' after ')'");
			
	/* function body */
	expr(&fcmp, ANY_PREC);
	emit_nullary(&fcmp, OP_RET);
	
	func_t* func = fcmp.func;
	
	/* push the function on the stack */
	emit_value(cmp, wrapfunc(func));
	
	/* write arguments to OP_PUSHFUNC */
	for (size_t i = 0;
		i < func->upvalues; i++)
	{
		cmp_upvalue_t* upval =
			&fcmp.upvals[i];
			
		func_write(cmp->func,
			upval->islocal,
			cmp->prev.line,
			cmp->ctx);
		func_write(cmp->func,
			upval->index,
			cmp->prev.line,
			cmp->ctx);
	}
	
	comp_copystate(cmp, &fcmp);
	comp_free(&fcmp);
	cmp->child = NULL;
}

/* parses an if/else expression */
void if_else(comp_t* cmp) {
	dbg_printfname();
	
	/* (condition) */
	eat(cmp, T_LPAREN,
		"expected '(' after 'if'");
	expr(cmp, ANY_PREC);
	eat(cmp, T_RPAREN,
		"expected ')' after if condition");
	
	/* jump to else branch if
	 * the condition is false */
	int then_addr =
		emit_jump(cmp, OP_JMPIFN);
		
	/* 'then' branch */
	emit_nullary(cmp, OP_POP); /* condition */
	expr(cmp, ANY_PREC);
	
	/* unconditionally jump over the 
	 * else branch */
	int else_addr =
		emit_jump(cmp, OP_JMP);
	patch_jump(cmp, then_addr);
	
	/* 'else' branch */
	emit_nullary(cmp, OP_POP); /* condition */
	if (match(cmp, T_ELSE))
		expr(cmp, ANY_PREC);
	else
		emit_value(cmp, wrapnil());
	
	/* skipped past else */
	patch_jump(cmp, else_addr);
}

/* parses a match expression */
void match_with(comp_t* cmp) {
	dbg_printfname();
	
	/* match target */
	expr(cmp, ANY_PREC);
	
	eat(cmp, T_WITH,
		"expected 'with' after expression");
	
	int arms = 0;
	int exits[MAX_MATCH_ARMS] = {0};
	int nexits = 0;
	while (match(cmp, T_PIPE)) {
		comp_simstack(cmp, +1);
		
		/* target == value */
		emit_nullary(cmp, OP_DUP);
		expr(cmp, ANY_PREC);
		emit_nullary(cmp, OP_EQ);
		
		/* skip arm if false */
		int skip = emit_jump(cmp, OP_JMPIFN);
		
		/* == arm matched! == */
		
		/* pop OP_EQ result */
		emit_nullary(cmp, OP_POP);
		
		/* pop target */
		emit_nullary(cmp, OP_POP);
		
		eat(cmp, T_MINUS_GREATER,
			"expected '->' after expression");
			
		expr(cmp, ANY_PREC);
		
		/* don't check any other arms */
		exits[nexits++] =
			emit_jump(cmp, OP_JMP);
		
		patch_jump(cmp, skip);
		
		/* == arm skipped == */
		
		/* pop OP_EQ result */
		emit_nullary(cmp, OP_POP);
		
		if (arms++ == MAX_MATCH_ARMS) {
			halt(cmp->ctx,
				"too many arms in match; "
				"the limit is %d",
				MAX_MATCH_ARMS);
			unreachable();
		}
	}
			
	eat(cmp, T_QUESTION,
		"'?' arm required as final arm of "
		"match expression");
		
	eat(cmp, T_MINUS_GREATER,
		"expected '->' after '?'");
	
	emit_nullary(cmp, OP_POP);
	expr(cmp, ANY_PREC);
	
	for (int i = 0; i < nexits; i++)
		patch_jump(cmp, exits[i]);
}

/* parses a block expression.
 * in essence, a block is a series of
 * expressions ultimately reduced down
 * to a single value */
void block(comp_t* cmp) {
	dbg_printfname();
	cmp->depth++;
	
	/* a block must return a value even
	 * when it contains zero expressions */
	if (match(cmp, T_RCURLY)) {
		emit_value(cmp, wrapnil());
		cmp->depth--;
		return;
	}
	
	while (!check(cmp, T_RCURLY)
		&& !check(cmp, T_EOF))
	{
		expr(cmp, ANY_PREC);
		
		/* pop the result of every expression
		 * in a block except for the last one,
		 * which becomes the return value
		 * of the entire block */
		if (!check(cmp, T_RCURLY))
			emit_nullary(cmp, OP_POP);
	}
	eat(cmp, T_RCURLY, "expected '}'");
	
	cmp->depth--;
	 
	/* count the number of local variables */
	int locals = 0;
	int index = cmp->nnames - 1;
	while (index >= 0 &&
		cmp->names[index].depth > cmp->depth)
	{
		locals++;
		index--;
	}
	
	if (locals == 0)
		return;
		
	/* the return value is on top of the
	 * stack so we need to hide it */
	emit_nullary(cmp, OP_HIDE_RET);
	
	/* pop the locals */
	while (cmp->nnames > 0 &&
		cmp->names[cmp->nnames - 1].depth
			> cmp->depth)
	{
		symbol_t* name =
			&cmp->names[cmp->nnames - 1];
		cmp->nnames--;
		
		if (name->capped)
			emit_nullary(cmp, OP_MOVE_HEAP);
		else
			emit_nullary(cmp, OP_POP);
	}
	
	/* restore the return value */
	emit_nullary(cmp, OP_SHOW_RET);
	
	/* shrink symbol table */
	cmp->names = arr_resize(
		cmp->names,
		symbol_t,
		cmp->nnames + locals,
		cmp->nnames,
		cmp->ctx);
}

void std_add(
	sylt_t* ctx,
	const char* name,
	value_t val)
{
	token_t token = (token_t){
		T_NAME, name, strlen(name), 1};
	add_symbol(ctx->cmp, token);
	gc_pause(ctx);
	emit_value(ctx->cmp, val);
	gc_resume(ctx);
}

void std_addf(
	sylt_t* ctx,
	const char* name,
	cfunc_t cfunc,
	int params)
{
	/* hide from GC */
	sylt_pushstring(ctx,
		string_lit(name, ctx));
	sylt_pushfunc(ctx, func_new(
		ctx, sylt_peekstring(ctx, 0)));
	sylt_peekfunc(ctx, 0)->cfunc = cfunc;
	sylt_peekfunc(ctx, 0)->params = params;
	
	std_add(ctx, name, sylt_peek(ctx, 0));
	
	/* safe */
	sylt_shrink(ctx, 2);
}

/* == GC == */

void gc_set_state(
	sylt_t* ctx, gc_state_t state)
{
	ctx->mem.gc.state = state;
}

void gc_pause(sylt_t* ctx) {
	gc_set_state(ctx, GC_STATE_PAUSED);
	ctx->mem.gc.npauses++;
}

void gc_resume(sylt_t* ctx) {
	if (ctx->mem.gc.state != GC_STATE_PAUSED)
		return;
		
	if (ctx->mem.gc.npauses > 0)
		ctx->mem.gc.npauses--;
	
	if (ctx->mem.gc.npauses == 0)
		gc_set_state(ctx, GC_STATE_IDLE);
}

void gc_mark(sylt_t*, const char*, int);
void gc_sweep(sylt_t*);

/* runs the garbage collector */
void gc_collect(
	sylt_t* ctx,
	const char* func_name,
	int line)
{
	#if DBG_NO_GC
	return;
	#endif
	
	if (ctx->mem.gc.state == GC_STATE_PAUSED)
		return;
	/*if (ctx->state == SYLT_STATE_COMPILING) return;*/
	assert(ctx->mem.gc.state
		== GC_STATE_IDLE);

	gc_mark(ctx, func_name, line);
	gc_sweep(ctx);
	
	ctx->mem.gc.nruns++;
	gc_set_state(ctx, GC_STATE_IDLE);
}

void gc_mark_vm_roots(sylt_t*);
void gc_mark_compiler_roots(sylt_t*);
void gc_trace_refs(sylt_t*);

/* marks all root objects */
void gc_mark(
	sylt_t* ctx,
	const char* func_name,
	int line)
{
	#if DBG_PRINT_GC_STATE
	sylt_dprintf(
		"\n     GC <%s:%d> ",
		func_name, line);
	#endif
	
	gc_set_state(ctx, GC_STATE_MARK);
	
	/* mark root objects */
	gc_mark_vm_roots(ctx);
	gc_mark_compiler_roots(ctx);
	
	#if DBG_PRINT_GC_STATE
	size_t roots = ctx->mem.gc.nmarked;
	#endif
	
	/* mark objects reachable from roots */
	gc_trace_refs(ctx);
	
	#if DBG_PRINT_GC_STATE
	size_t refs = ctx->mem.gc.nmarked - roots;
	
	sylt_dprintf(
		"%ld root%s + %ld ref%s, ",
		roots,
		(roots == 1) ? "" : "s",
		refs,
		(refs == 1) ? "" : "s");
	#endif
	
	gc_free(ctx->mem.gc.marked);
	ctx->mem.gc.marked = NULL;
	ctx->mem.gc.nmarked = 0;
}

/* marks all roots reachable from
 * the VM */
void gc_mark_vm_roots(sylt_t* ctx) {
	vm_t* vm = ctx->vm;
	if (!vm)
		return;
	
	/* stack */
	value_t* v = vm->stack;
	for (; v < vm->sp; v++)
		val_mark(*v, ctx);
		
	/* call stack */
	for (size_t i = 0; i < vm->nframes; i++) {
		/*obj_mark((obj_t*)vm->frames[i].func,
			ctx);*/
		obj_mark((obj_t*)vm->frames[i].cls,
			ctx);
	}
	
	/* open upvalues */
	upvalue_t* upval = vm->openups;
	for (; upval; upval = upval->next)
		obj_mark((obj_t*)upval, ctx);
	
	/* return value */
	val_mark(vm->hidden, ctx);
}

/* marks all roots that can be reached from
 * the compiler */
void gc_mark_compiler_roots(sylt_t* ctx) {
	if (ctx->state != SYLT_STATE_COMPILING)
		return;
	assert(ctx->cmp);
	
	/* compiler stack */
	comp_t* cmp = ctx->cmp;
	do {
		obj_mark((obj_t*)cmp->func, ctx);
		obj_mark((obj_t*)cmp->src, ctx);
		cmp = cmp->child;
	} while (cmp);
}

/* iterates through all marked roots
 * and marks any child objects reachable
 * from them */
void gc_trace_refs(sylt_t* ctx) {
	size_t n = ctx->mem.gc.nmarked;
	while (n > 0) {
		obj_t* obj = ctx->mem.gc.marked[--n];
		obj_deep_mark(obj, ctx);
	}
}

/* frees all unreachable objects */
void gc_sweep(sylt_t* ctx) {
	gc_set_state(ctx, GC_STATE_SWEEP);
	obj_t* prev = NULL;
	obj_t* obj = ctx->mem.objs;
	
	#if DBG_PRINT_GC_STATE
	size_t n = 0;
	obj_t* o = obj;
	while (o) {
		if (o->marked) {
			o = o->next;
			continue;
		}
		
		o = o->next;
		n++;
	}
	sylt_dprintf("%ld swept\n", n);
	#endif
	
	while (obj) {
		if (obj->marked) {
			/* object is reachable; unmark it
		 	* and skip to the next */
			obj->marked = false;
			prev = obj;
			obj = obj->next;
			continue;
		}
		
		/* object is not reachable 
		 * (except from here...) */
		obj_t* unreached = obj;
		
		/* delete it from the linked list */
		obj = obj->next;
		if (prev)
			prev->next = obj;
		else
			ctx->mem.objs = obj;
		
		/* free its memory */
		obj_free(unreached, ctx);
	}
}

/* called on shutdown; frees all objects */
void gc_free_all(sylt_t* ctx) {
	obj_t* obj = ctx->mem.objs;
	while (obj) {
		obj_t* next = obj->next;
		obj_free(obj, ctx);
		obj = next;
	}
}

string_t* load_file(
	const char* path, sylt_t* ctx)
{
	FILE* fp = fopen(path, "rb");
	if (!fp) {
		halt(ctx, E_IO(
			"failed to open", path));
		return NULL;
	}
	
	/* seek EOF to find the file size */
	fseek(fp, 0, SEEK_END);
	size_t len = ftell(fp);
	fseek(fp, 0, SEEK_SET);
	
	/* read the file contents */
	string_t* str = string_new(
		NULL, len + 1, false, ctx);
	fread(str->bytes, 1, len, fp);
	fclose(fp);
	
	str->bytes[len] = '\0';
	return str;
}

/* halts with an error message */
void halt(sylt_t* ctx, const char* fmt, ...) {
	/* print formatted message into buffer */
	char msg[MAX_ERRMSGLEN];
	va_list args;
	va_start(args, fmt);
	vsnprintf(msg, MAX_ERRMSGLEN, fmt, args);
	va_end(args);
	
	sylt_eprintf("\n");
	
	/* print message prefix */
	int state = (ctx) ? ctx->state : -1;
	switch (state) {
	case SYLT_STATE_COMPILING: {
		/* find the deepest compiler */
		comp_t* cmp = ctx->cmp;
		while (cmp->child)
			cmp = cmp->child;
		
		sylt_eprintf("error in %.*s:%d: ",
			(int)cmp->func->name->len,
			cmp->func->name->bytes,
			cmp->prev.line);
		break;
	}
	case SYLT_STATE_EXEC: {
		const func_t* func =
			ctx->vm->fp->func;
		
		size_t addr =
			(size_t)(ctx->vm->fp->ip -
				func->code - 1);
		uint32_t line = func->lines[addr];
		
		sylt_eprintf("error in %.*s:%d: ",
			(int)func->name->len,
			func->name->bytes,
			line);
		break;
	}
	default: sylt_eprintf("error: ");
	}
	
	sylt_eprintf("%s\n", msg);
	longjmp(err_jump, 1);
}

sylt_t* sylt_new(void) {
	if (setjmp(err_jump))
		return NULL;
	
	sylt_t* ctx = NULL;
	ptr_alloc(ctx, sylt_t, NULL);
	sylt_set_state(ctx, SYLT_STATE_ALLOC);
		
	ptr_alloc(ctx->vm, vm_t, ctx);
	vm_init(ctx->vm, ctx);
	
	ptr_alloc(ctx->cmp, comp_t, ctx);
	
	/* init memory */
	ctx->mem.objs = NULL;
	/* has to be done manually when
	 * allocating ctx struct itself */
	ctx->mem.bytes += sizeof(sylt_t);
	ctx->mem.highest = 0;
	ctx->mem.count = 0;
	
	/* init GC state */
	ctx->mem.gc.marked = NULL;
	ctx->mem.gc.nmarked = 0;
	ctx->mem.gc.nruns = 0;
	ctx->mem.gc.npauses = 0;
	
	sylt_set_state(ctx, SYLT_STATE_INIT);
	return ctx;
}

void sylt_free(sylt_t* ctx) {
	if (!ctx)
		return;
	
	sylt_set_state(ctx, SYLT_STATE_FREEING);
	
	/* free all unreleased objects */
	gc_free_all(ctx);
	
	/* free the VM */
	if (ctx->vm) {
		vm_free(ctx->vm);
		ptr_free(ctx->vm, vm_t, ctx);
		ctx->vm = NULL;
	}
	
	/* free compiler */
	if (ctx->cmp) {
		comp_free(ctx->cmp);
		ptr_free(ctx->cmp, comp_t, ctx);
		ctx->cmp = NULL;
	}
	
	/* ensure that no memory was leaked */
	ptrdiff_t bytesleft =
		ctx->mem.bytes - sizeof(sylt_t);
	if (bytesleft)
		sylt_dprintf("%ld bytes leaked\n",
			bytesleft);
	assert(!bytesleft);
	
	dbg_print_mem_stats(ctx);
	dbg_print_mem_sizes();
	
	sylt_set_state(ctx, SYLT_STATE_FREE);
	ptr_free(ctx, sylt_t, ctx);
}

void compile(sylt_t* ctx) {
	/* init a fresh compiler */
	comp_init(ctx->cmp, NULL, NULL, ctx);
	
	#if DBG_PRINT_SOURCE
	puts((char*)ctx->cmp->src->bytes);
	#endif
	
	sylt_set_state(ctx, SYLT_STATE_COMPILING);
	
	/* load standard library */
	ctx->cmp->prev.line = 1; /* TODO: hack */
	load_stdlib(ctx);
	
	/* scan initial token for lookahead */
	ctx->cmp->cur = scan(ctx->cmp);
	
	/* parse the entire source */
	while (!check(ctx->cmp, T_EOF)) {
		expr(ctx->cmp, ANY_PREC);
		emit_nullary(ctx->cmp, OP_POP);
	}
	
	emit_nullary(ctx->cmp, OP_RET);
	sylt_pushfunc(ctx, ctx->cmp->func);
	sylt_set_state(ctx, SYLT_STATE_COMPILED);
}

/* compiles and runs a sylt program from
 * a string, returning true if successful */
bool sylt_dostring(
	sylt_t* ctx, const char* src)
{
	if (!ctx || !src)
		return false;
		
	if (setjmp(err_jump)) {
		sylt_free(ctx);
		ctx = sylt_new();
		return false;
	}
	
	/* push the source code + program name */
	sylt_pushstring(ctx,
		string_lit(src, ctx));
	sylt_pushstring(ctx,
		string_lit("input", ctx));
	
	compile(ctx);
	vm_exec(ctx->vm);
	return true;
}

/* compiles and runs a sylt program from
 * file, returning true if successful */
bool sylt_dofile(
	sylt_t* ctx, const char* path)
{
	if (!ctx || !path)
		return false;
	
	if (setjmp(err_jump)) {
		sylt_free(ctx);
		ctx = sylt_new();
		return false;
	}
	
	/* push the source code + file name */
	sylt_pushstring(ctx,
		load_file(path, ctx));
	sylt_pushstring(ctx,
		string_lit(path, ctx));
	
	compile(ctx);
	vm_exec(ctx->vm);
	return true;
}

void sylt_test(sylt_t* ctx) {
	/* empty input */
	//sylt_dostring(ctx, "");
	
	/* make sure ensure() works */
	//assert(!sylt_dostring(
	//	ctx, "ensure(false)"));
	
	/* run lang tests */
	sylt_dofile(ctx, "tests.sylt");
}

int main(int argc, char *argv[]) {
	if (argc == 1) {
		sylt_printf("Usage: sylt [file]\n");
		return EXIT_SUCCESS;
	}
	
	printf("%d\n", nextpow2(0));
	printf("%d\n", nextpow2(1));
	printf("%d\n", nextpow2(7));
	printf("%d\n", nextpow2(500));
	
	sylt_t* ctx = sylt_new();
	//sylt_test(ctx);
	sylt_dofile(ctx, argv[1]);
	sylt_free(ctx);
	
	return EXIT_SUCCESS;
}
