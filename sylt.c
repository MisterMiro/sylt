#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <setjmp.h>
#include <ctype.h>
#include <math.h>

#define SYLT_VERSION_STR "sylt dev"
#define SYLT_VERSION_MAJ 0
#define SYLT_VERSION_MIN 0
#define SYLT_VERSION_REV 0

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
#define SYLT_INIT_STACK 8192

/* == debug flags ==
 * all of these should be set to 0 in
 * release builds */

/* enables assertions */
#define DBG_ASSERTIONS 1

/* disables garbage collection, though
 * memory will still be freed at shutdown
 * (sylt_free) to prevent memory leaks */
#define DBG_NO_GC 0

/* triggers the GC on every allocation,
 * super slow but good for bug hunting */
#define DBG_GC_EVERY_ALLOC 1

#define DBG_PRINT_SYLT_STATE 0
#define DBG_PRINT_GC_STATE 0
#define DBG_PRINT_SOURCE 0
#define DBG_PRINT_TOKENS 0
#define DBG_PRINT_NAMES 0
#define DBG_PRINT_AST 0
#define DBG_PRINT_CODE 0
#define DBG_PRINT_DATA 0
#define DBG_PRINT_STACK 0
#define DBG_PRINT_MEM_STATS 0

static void dbg_print_flags(void) {
	#define pflag(flag) \
		if (flag) \
			sylt_dprintf( \
				"note: %s is set\n", #flag)
			
	pflag(DBG_ASSERTIONS);
	pflag(DBG_NO_GC);
	pflag(DBG_GC_EVERY_ALLOC);
	pflag(DBG_PRINT_SYLT_STATE);
	pflag(DBG_PRINT_GC_STATE);
	pflag(DBG_PRINT_SOURCE);
	pflag(DBG_PRINT_TOKENS);
	pflag(DBG_PRINT_NAMES);
	pflag(DBG_PRINT_AST);
	pflag(DBG_PRINT_CODE);
	pflag(DBG_PRINT_DATA);
	pflag(DBG_PRINT_STACK);
	pflag(DBG_PRINT_MEM_STATS);
			
	#undef pflag
}

/* == optimizations == */

#define OPTZ_DEDUP_CONSTANTS 1
#define OPTZ_SPECIAL_PUSHOPS 1

/* == constant limits == */

#define MAX_CODE (UINT16_MAX + 1)
#define MAX_DATA (UINT8_MAX + 1)
#define MAX_STACK (UINT8_MAX + 1)
#define MAX_LINES UINT32_MAX
#define MAX_JUMP UINT16_MAX
#define MAX_CFRAMES 8192
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

#if DBG_PRINT_SYLT_STATE
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
#endif

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
	/* bytes needed to trigger a cycle */
	size_t trigger;
	/* number of succesful gc_collect calls */
	size_t cycles;
	/* keeps track of nested pauses */
	size_t pause_depth;
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
	/* number of objects allocated */
	size_t objcount;
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

/* == API == */

sylt_t* sylt_new(void);
void sylt_free(sylt_t* ctx);

bool sylt_dostring(sylt_t* ctx,
	const char* src);
bool sylt_dofile(sylt_t* ctx,
	const char* path);
	
/* == error messages == */

void halt(sylt_t*, const char*, ...);

#define E_OUTOFMEM \
	"out of memory"
#define E_CODELIMIT \
	"bytecode limit of %d reached", MAX_CODE
#define E_DATALIMIT \
	"data limit of %d reached", MAX_DATA
#define E_STACKLIMIT \
	"stack limit of %d reached", MAX_STACK
#define E_LINELIMIT \
	"line limit of %d reached", MAX_LINES
#define E_JUMPLIMIT \
	"jump distance too far (>%d)", MAX_JUMP
#define E_IO(msg, path) \
	"%s %s", (msg), (path)
#define E_UNEXPECTEDCHAR(c) \
	"unexpected character '%c'", (c)
#define E_TYPE(ex, got) \
	"expected value of type %s, got %s", \
	user_type_name(ex), \
	user_type_name(got)
#define E_UNDEFINED(name) \
	"undefined variable '%.*s'", \
	(name->len), (name->bytes)
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
#define E_DIVBYZERO \
	"attempted to divide by zero"
#define E_STACKOVERFLOW \
	"call stack overflow"
#define E_INDEX(len, index) \
	"index out of range, len: %d, i=%d", \
	(len), (index)
#define E_WRONGARGC(need, got) \
	"expected %d argument%s, got %d", \
	(need), ((need) == 1 ? "" : "s"), (got)

/* triggers one of each error */
void test_errors(sylt_t* ctx) {
	sylt_dprintf(
		"Testing error messages, "
		"please ignore:\n");
	
	const char* src;
	
	/* unexpected character */
	src = "`";
	assert(!sylt_dostring(ctx, src));
	
	/* type error */
	src = "1 + true";
	assert(!sylt_dostring(ctx, src));
	
	/* undefined variable */
	src = "1 + milk";
	assert(!sylt_dostring(ctx, src));
	
	/* unknown escape sequence */
	src = "\" \\w \"";
	assert(!sylt_dostring(ctx, src));
	
	/* unterminated string literal */
	src = "\"";
	assert(!sylt_dostring(ctx, src));
	
	/* division by zero */
	src = "1 / 0";
	assert(!sylt_dostring(ctx, src));
	
	/* stack overflow */
	src =
		"let rec(i, n) = "
		"	if (i < n) "
		"		rec(i + 1, n) "
		"rec(0, 8193)";
	assert(!sylt_dostring(ctx, src));
	
	/* index */
	src =
		"let list = []"
		"list[0]";
	assert(!sylt_dostring(ctx, src));
	
	/* wrong argument count */
	src = "ensure(1, 2)";
	assert(!sylt_dostring(ctx, src));
}

/* == memory == */

/* initial capacity for dynamic arrays.
 * higher values lead to less allocations
 * but more wasted memory and vice versa.
 * should be a power of two */
#define INIT_ARRAY_CAP 8

/* returns n rounded up to the nearest
 * power of two. this is used when resizing
 * dynamic arrays in order to avoid
 * having to store separate values for
 * the length and capacity */
static unsigned nextpow2(unsigned n) {
	if (n > 0 && n < INIT_ARRAY_CAP)
		return INIT_ARRAY_CAP;
	n--;
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n |= n >> 8;
	n |= n >> 16;
	return ++n;
}

/* number of bytes that need to be allocated
 * before triggering the first GC cycle */
#define GC_INIT_THRESHOLD (1024 * 1024)

/* how much to multiply the GC threshold
 * by after each collection cycle */
#define GC_THRESHOLD_CLIMB 2

/* the GC needs to allocate some memory
 * while working */
#define gc_realloc realloc
#define gc_free free

void gc_pause(sylt_t*);
void gc_resume(sylt_t*);
void gc_collect(sylt_t*, const char*, int);

/* handles all memory allocation
 * (see macros below) */
void* ptr_resize(
	void* p,
	size_t os,
	size_t ns,
	const char* p_name,
	const char* func_name,
	int line,
	sylt_t* ctx)
{
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
		#else
		bool trigger_gc = ns > os &&
			ctx->mem.bytes >
			ctx->mem.gc.trigger;
		
		if (trigger_gc)
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
	OP_POP_HEAP,
	OP_DUP,
	/* memory */
	OP_LOAD,
	OP_STORE,
	OP_LOAD_LIST,
	OP_STORE_LIST,
	OP_LOAD_UPVAL,
	OP_STORE_UPVAL,
	OP_LOAD_RET,
	OP_STORE_RET,
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
	[OP_PUSH_NIL] = {"push_nil", 0, +1},
	[OP_PUSH_TRUE] = {"push_true", 0, +1},
	[OP_PUSH_FALSE] = {"push_false", 0, +1},
	[OP_PUSH_LIST] = {"push_list", 1, +1},
	[OP_PUSH_FUNC] = {"push_func", 1, +1},
	[OP_POP] = {"pop", 0, -1},
	[OP_POP_HEAP] = {"pop_heap", 0, -1},
	[OP_DUP] = {"dup", 0, +1},
	[OP_LOAD] = {"load", 1, +1},
	[OP_STORE] = {"store", 1, 0},
	[OP_LOAD_LIST] = {"load_list", 0, -1},
	[OP_STORE_LIST] = {"store_list", 0, 0},
	[OP_LOAD_UPVAL] = {"load_upval", 1, +1},
	[OP_STORE_UPVAL] = {"store_upval", 1, 0},
	[OP_LOAD_RET] = {"load_ret", 0, +1},
	[OP_STORE_RET] = {"store_ret", 0, -1},
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
	/* null terminated list of characters */
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
	/* full name, including file name */
	string_t* path;
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
#define wrapupvalue(v) \
	(value_t){TYPE_UPVALUE, \
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
#define getupvalue(v) \
	((upvalue_t*)(v).data.obj)

#define typecheck(ctx, v, t) \
	if (v.tag != t) \
		halt(ctx, E_TYPE(t, v.tag))
#define typecheck2(ctx, a, b, t) \
	typecheck(ctx, a, t); \
	typecheck(ctx, b, t)
	
/* == public stack API == */
/* these are useful even outside of the VM,
 * for making objects visible to the GC */

static inline void vm_push(
	struct vm_s*, struct value_s);
static inline value_t vm_pop(
	struct vm_s*);
static inline value_t vm_peek(
	const struct vm_s*, int);
static inline void vm_shrink(
	struct vm_s*, int);

/* for pushing values on the stack */
#define sylt_push(ctx, v) \
	vm_push(ctx->vm, v)
#define sylt_pushnum(ctx, v) \
	sylt_push(ctx, wrapnum(v))
#define sylt_pushstring(ctx, v) \
	sylt_push(ctx, wrapstring(v))
#define sylt_pushfunc(ctx, v) \
	sylt_push(ctx, wrapfunc(v))
#define sylt_pushclosure(ctx, v) \
	sylt_push(ctx, wrapclosure(v))

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
#define sylt_peekclosure(ctx, n) \
	getclosure(sylt_peek(ctx, n))

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
	obj_t* obj = ptr_resize(
		NULL, 0, size,
		"<obj>", func_name, line,ctx);
	obj->tag = tag;
	obj->marked = false;
	obj->next = ctx->mem.objs;
	ctx->mem.objs = obj;
	ctx->mem.objcount++;
	
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
			nextpow2(list->len),
			ctx);
		
		ptr_free(list, list_t, ctx);
		break;
	}
	case TYPE_STRING: {
		string_t* str = (string_t*)obj;
		arr_free(str->bytes,
			uint8_t,
			str->len + 1,
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
		obj_mark((obj_t*)func->path, ctx);
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
value_t* list_get(
	const list_t* ls, int index, sylt_t* ctx)
{
	/* allow negative indexing */
	if (index < 0) {
		int mod = ls->len + index;
		if (mod > ls->len - 1) {
			halt(ctx, E_INDEX(ls->len, mod));
			unreachable();
		}
		return &ls->items[mod];
	}
	
	if (ls->len == 0 || index > ls->len - 1) {
		halt(ctx, E_INDEX(ls->len, index));
		unreachable();
	}
	
	return &ls->items[index];
}

/* appends an item to the end of the list */
void list_push(
	list_t* ls, value_t val, sylt_t* ctx)
{
	/* grow allocation */
	size_t oldsz = nextpow2(ls->len);
	size_t newsz = nextpow2(ls->len + 1);
	if (newsz > oldsz) {
		ls->items = arr_resize(
			ls->items, value_t,
			oldsz, newsz, ctx);
	}
	
	ls->items[ls->len++] = val;
}

/* returns and then removes the last item
 * from the list */
value_t list_pop(list_t* ls, sylt_t* ctx) {
	if (ls->len == 0)
		return wrapnil();
	value_t last = *list_get(ls, -1, ctx);
	
	/* shrink allocation */
	size_t oldsz = nextpow2(ls->len);
	size_t newsz = nextpow2(ls->len - 1);
	if (newsz < oldsz) {
		ls->items = arr_resize(
			ls->items, value_t,
			oldsz, newsz, ctx);
	}
	
	ls->len--;
	return last;
}

/* == string == */

/* creates a new string */
string_t* string_new(
	uint8_t* bytes, size_t len, sylt_t* ctx)
{
	string_t* str = (string_t*)obj_new(
		sizeof(string_t), TYPE_STRING, ctx);
	sylt_pushstring(ctx, str); /* GC */
		
	str->bytes = NULL;
	arr_alloc(
		str->bytes, uint8_t, len + 1, ctx);
	
	if (bytes)
		memcpy(str->bytes, bytes, len);
	
	str->bytes[len] = '\0';
	str->len = len;
	return sylt_popstring(ctx);
}

/* helper function for creating a new 
 * string object from a C string literal */
string_t* string_lit(
	const char* lit, sylt_t* ctx)
{
	return string_new(
		(uint8_t*)lit, strlen(lit), ctx);
}

/* creates a new formatted string */
string_t* string_fmt(
	sylt_t* ctx, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	size_t len = vsnprintf(
		NULL, 0, fmt, args);
	
	string_t* str = string_new(
		NULL, len, ctx);
	vsnprintf((char*)str->bytes,
		len + 1, fmt, args);
	va_end(args);
	
	return str;
}

/* returns true if a == b */
bool string_eq(
	const string_t* a,
	const string_t* b)
{
	if (a == b)
		return true;
		
	if (a->len != b->len)
		return false;
		
	return memcmp(a->bytes, b->bytes, a->len)
		== 0;
}

/* concatenates two strings */
string_t* string_concat(
	const string_t* a,
	const string_t* b,
	sylt_t* ctx)
{
	size_t len = a->len + b->len;
	string_t* result = string_new(
		NULL, len, ctx);
	
	memcpy(result->bytes, a->bytes, a->len);
	memcpy(result->bytes + a->len,
		b->bytes, b->len);
	
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

void string_print(string_t* str) {
	sylt_printf("%.*s",
		(int)str->len, str->bytes);
}

void string_eprint(string_t* str) {
	sylt_eprintf("%.*s",
		(int)str->len, str->bytes);
}

void string_dprint(string_t* str) {
	sylt_dprintf("%.*s",
		(int)str->len, str->bytes);
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
	func->path = name;
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
		unreachable();
	}
	
	if (func->nlines >= MAX_LINES) {
		halt(ctx, E_LINELIMIT);
		unreachable();
	}
	
	/* write byte */
	size_t oldsz = nextpow2(func->ncode);
	size_t newsz = nextpow2(func->ncode + 1);
	if (newsz > oldsz) {
		func->code = arr_resize(
			func->code, uint8_t,
			oldsz, newsz, ctx);
	}
	func->code[func->ncode++] = byte;
	
	/* write corresponding line number */
	oldsz = nextpow2(func->nlines);
	newsz = nextpow2(func->nlines + 1);
	if (newsz > oldsz) {
		func->lines = arr_resize(
			func->lines, uint32_t,
			oldsz, newsz, ctx);
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
	case TYPE_STRING:
		return string_eq(
			getstring(a), getstring(b));
	case TYPE_FUNCTION:
	case TYPE_CLOSURE:
	case TYPE_UPVALUE:
		return getobj(a) == getobj(b);
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
		gc_pause(ctx);
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
		
		string_t* result = string_concat(
			str,
			string_lit("]", ctx),
			ctx);
		
		sylt_pop(ctx); /* GC */
		gc_resume(ctx);
		return result;
	}
	case TYPE_STRING: {
		return getstring(val);
	}
	case TYPE_CLOSURE: {
		return
			getclosure(val)->func->name;
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
		string_t* str =
			string_new(NULL, 0, ctx);
		
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
	string_print(str);
}

/* == virtual machine == */

void vm_ensure_stack(vm_t*, int);

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
	vm_ensure_stack(vm, SYLT_INIT_STACK);
}

void vm_free(vm_t* vm) {
	arr_free(vm->stack, 
		value_t,
		vm->maxstack,
		vm->ctx);
}

/* VM macros */
#define read8() *vm->fp->ip++
#define read16() \
	(vm->fp->ip += 2, (uint16_t) \
		((vm->fp->ip[-2] << 8) \
			| vm->fp->ip[-1]))
#define push(v) vm_push(vm, (v))
#define pop() vm_pop(vm)
#define peek(n) vm_peek(vm, (n))
#define shrink(n) vm_shrink(vm, (n))
#define func_stack() \
	(vm->stack + vm->fp->offs)

void dbg_print_mem_stats(sylt_t* ctx) {
	#if DBG_PRINT_MEM_STATS
	sylt_dprintf(
		"[memory]\n"
		"- leaked: %ld bytes\n"
		"- highest usage: %ld bytes\n"
		"- allocations: %ld\n"
		"- objects: %ld\n"
		"- gc cycles: %ld\n",
		ctx->mem.bytes - sizeof(sylt_t),
		ctx->mem.highest,
		ctx->mem.count,
		ctx->mem.objcount,
		ctx->mem.gc.cycles);
	#endif
}

void dbg_print_header(
	const vm_t* vm, const closure_t* cls)
{
	const func_t* func = cls->func;
	
	sylt_dprintf("\n-> ");
	sylt_dprintf("%-29.*s",
		(int)func->name->len,
		func->name->bytes);
	string_dprint(func->path);
	sylt_dprintf("\n");
	
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
	sylt_dprintf("  data:\n");
	for (size_t i = 0; i < func->ndata; i++)
	{
		string_t* str = val_tostring_opts(
			func->data[i],
			true,
			24,
			vm->ctx);
		
		sylt_dprintf("    #%-2ld -- ", i);
		string_dprint(str);
		sylt_dprintf("\n");
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
	
	/* line number */
	const uint32_t* lines =
		vm->fp->func->lines;
	uint32_t line = lines[addr];
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
	
	value_t* start = func_stack();
	
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
	assert(ptr >= vm->stack);
	assert(ptr <= vm->sp - 1);
	return *ptr;
}

void vm_math(vm_t* vm, op_t opcode);

/* returns the value referenced by
 * an upvalue */
value_t vm_load_upval(
	vm_t* vm, upvalue_t* upval)
{
	if (upval->index == -1)
		return upval->closed;
	return vm->stack[upval->index];
}

/* sets the value referenced by an upvalue */
void vm_store_upval(
	vm_t* vm, upvalue_t* upval, value_t val)
{
	if (upval->index == -1) {
		upval->closed = val;
		return;
	}
	
	vm->stack[upval->index] = val;
}

/* captures a stack variable into an
 * upvalue */
upvalue_t* vm_cap_upval(
	vm_t* vm, size_t index)
{
	/* check if there already exists an
	 * upvalue for the given index */
	upvalue_t* prev = NULL;
	upvalue_t* cur = vm->openups;
	while (cur && cur->index > index) {
		prev = cur;
		cur = cur->next;
	}
	
	/* return previous upvalue */
	if (cur && cur->index == index)
		return cur;
	
	/* create a fresh upvalue */
	upvalue_t* fresh = upvalue_new(
		vm->ctx, index);
	
	/* insert it into linked list */
	fresh->next = cur;
	if (!prev)
		vm->openups = fresh;
	else
		prev->next = fresh;
	
	return fresh;
}

void vm_close_upvals(
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

/* some extra stack space for hiding stuff
 * from the GC */
#define EXTRA_STACK 32

/* grows the stack if necessary */
void vm_ensure_stack(vm_t* vm, int needed) {
	needed += EXTRA_STACK;
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

void sylt_call(sylt_t*, int);

void vm_load(vm_t* vm) {
	assert(peek(0).tag == TYPE_FUNCTION);
	const func_t* entry = getfunc(peek(0));
	assert(entry);
	
	sylt_pushclosure(vm->ctx,
		closure_new(vm->ctx, entry));
	sylt_call(vm->ctx, 0);
}

void vm_exec(vm_t* vm, bool stdlib_call) {
	sylt_set_state(vm->ctx, SYLT_STATE_EXEC);
	assert(vm->nframes > 0 && vm->fp);
	
	#if DBG_PRINT_CODE
	dbg_print_header(vm, vm->fp->cls);
	#endif
	
	for (;;) {
		uint8_t op = read8();
		
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
				data[read8()];
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
			uint8_t len = read8();
			list_t* ls = list_new(vm->ctx);
			
			push(wraplist(ls)); /* GC */
			for (int i = len; i > 0; i--)
				list_push(
					ls, peek(i), vm->ctx);
			pop(); /* GC */
			
			shrink(len);
			push(wraplist(ls));
			break;
		}
		case OP_PUSH_FUNC: {
			gc_pause(vm->ctx);
			
			func_t* func = getfunc(
				vm->fp->func->data[read8()]);
			
			/* wrap it in a closure */
			closure_t* cls =
				closure_new(vm->ctx, func);
			push(wrapclosure(cls));
			
			arr_alloc(
				cls->upvals,
				upvalue_t*,
				func->upvalues,
				vm->ctx);
			cls->nupvals = func->upvalues;
			
			/* read upvalues */
			for (size_t i = 0;
				i < cls->nupvals; i++)
			{
				uint8_t is_local = read8();
				uint8_t index = read8();
				upvalue_t* upval = NULL;
				
				if (is_local)
					upval = vm_cap_upval(vm,
						vm->fp->offs + index);
				else
					upval = vm->fp->
						cls->upvals[index];
				
				cls->upvals[i] = upval;
			}
			
			gc_resume(vm->ctx);
			break;
		}
		case OP_POP: {
			pop();
			break;
		}
		case OP_POP_HEAP: {
			vm_close_upvals(vm,
				vm->sp - vm->stack - 1);
			pop();
			break;
		}
		case OP_DUP: {
			value_t val = peek(0);
			push(val);
			break;
		}
		/* == memory == */
		case OP_LOAD: {
			push(func_stack()[read8()]);
			break;
		}
		case OP_STORE: {
			func_stack()[read8()] = peek(0);
			break;
		}
		case OP_LOAD_LIST: {
			typecheck(
				vm->ctx, peek(0), TYPE_NUM);
			typecheck(
				vm->ctx, peek(1), TYPE_LIST);
				
			sylt_num_t index = getnum(pop());
			list_t* ls = getlist(pop());
			
			push(*list_get(
				ls, index, vm->ctx));
			break;
		}
		case OP_STORE_LIST: {
			value_t val = pop();
			
			typecheck(
				vm->ctx, peek(0), TYPE_NUM);
			typecheck(
				vm->ctx, peek(1), TYPE_LIST);
				
			sylt_num_t index = getnum(pop());
			list_t* ls = getlist(pop());
			
			*list_get(ls, index, vm->ctx)
				= val;
			push(val);
			break;
		}
		case OP_LOAD_UPVAL: {
			value_t val = vm_load_upval(vm,
				vm->fp->cls->upvals[read8()]);
			push(val);
			break;
		}
		case OP_STORE_UPVAL: {
			vm_store_upval(vm,
				vm->fp->cls->upvals[read8()],
				peek(0));
			break;
		}
		case OP_LOAD_RET: {
			push(vm->hidden);
			break;
		}
		case OP_STORE_RET: {
			vm->hidden = pop();
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
				vm->ctx, peek(0), TYPE_NUM);
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
				vm->ctx, peek(0), TYPE_BOOL);
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
			sylt_call(vm->ctx, read8());
			break;
		}
		case OP_RET: {
			value_t retval = pop();
			vm_close_upvals(vm, vm->fp->offs);
			vm->sp = func_stack() - 1;
			push(retval);
			
			vm->nframes--;
			if (vm->nframes == 0) {
				sylt_set_state(vm->ctx,
					SYLT_STATE_DONE);
				return;
			}
	
			vm->fp =
				&vm->frames[vm->nframes - 1];
				
			if (stdlib_call) {
				return;
			}
			
			#if DBG_PRINT_CODE
			dbg_print_header(
				vm, vm->fp->cls);
			#endif
			break;
		}
		default: unreachable();
		}
	}
}

void vm_math(vm_t* vm, op_t opcode) {
	typecheck(vm->ctx, peek(0), TYPE_NUM);
	typecheck(vm->ctx, peek(1), TYPE_NUM);
	sylt_num_t b = getnum(pop());
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

void sylt_call(sylt_t* ctx, int argc) {
	vm_t* vm = ctx->vm;
	
	typecheck(
		vm->ctx, peek(argc), TYPE_CLOSURE);
	closure_t* cls = getclosure(peek(argc));
	const func_t* func = cls->func;
				
	if (func->params != argc) {
		halt(vm->ctx, E_WRONGARGC(
			func->params, argc));
			unreachable();
	}
			
	if (func->cfunc) {
		gc_pause(vm->ctx);
				
		/* calling a C function
		 * does not involve the
		 * sylt callstack */
		value_t result = func->cfunc(vm->ctx);
		shrink(argc + 1);
		push(result);
				
		#if DBG_PRINT_CODE
		sylt_dprintf("\n");
		#endif
				
		gc_resume(vm->ctx);
		return;
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
	size_t needed = used + func->slots;
	vm_ensure_stack(vm, needed);
			
	/* push a new call frame */
	cframe_t frame;
	frame.func = func;
	frame.cls = cls;
	frame.ip = func->code;
	frame.offs = used - argc;
	vm->frames[vm->nframes++] = frame;
	vm->fp = &vm->frames[vm->nframes - 1];
				
	#if DBG_PRINT_CODE
	dbg_print_header(vm, vm->fp->cls);
	#endif
}

#undef read8
#undef read16
#undef push
#undef pop
#undef peek
#undef shrink
#undef func_stack

/* == standard library == */

#define argc() (ctx->vm->fp->ip[-1])
#define arg(n) \
	vm_peek(ctx->vm, argc() - (n) - 1)

#define boolarg(n) getbool(arg(n))
#define numarg(n) getnum(arg(n))
#define listarg(n) getlist(arg(n))
#define stringarg(n) getstring(arg(n))
#define closurearg(n) getclosure(arg(n))

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

/* calls f n times with n as argument */
value_t std_rep(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_CLOSURE);
	
	int n = numarg(0);
	closure_t* f = closurearg(1);
	
	for (int i = 0; i < n; i++) {
		sylt_pushclosure(ctx, f);
		sylt_pushnum(ctx, i);
		sylt_call(ctx, 1);
		vm_exec(ctx->vm, true);
	}
	
	return wrapnil();
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

/* == string lib == */

/* returns all chars (bytes) in a string
 * as a list */
value_t std_chars(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_STRING);
	string_t* str = stringarg(0);
	list_t* ls = list_new(ctx);
	
	for (int i = 0; i < str->len; i++) {
		string_t* ch = string_new(
			&str->bytes[i], 1, ctx);
		list_push(ls, wrapstring(ch), ctx);
	}
	
	return wraplist(ls);
}

/* == math lib == */

/* returns true if a is nearly equal
 * to b, within a tolerance of epsilon */
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

/* returns -1 if x is negative, 0 if it's
 * zero, and 1 if positive */
value_t std_signum(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	sylt_num_t x = numarg(0);
	if (x < 0)
		return wrapnum(-1);
	if (x == 0)
		return wrapnum(0);
	return wrapnum(1);
}

/* returns the absolute value of x */
value_t std_abs(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	sylt_num_t result =
		num_func(fabsf, fabs)(numarg(0));
	return wrapnum(result);
}

/* returns the exponent to which base
 * needs to be raised in order to 
 * produce x */
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

/* returns base raised to the power of exp */
value_t std_raise(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_NUM);
	
	sylt_num_t base = numarg(0);
	sylt_num_t exp = numarg(1);
	sylt_num_t result =
		num_func(powf, pow)(base, exp);
	return wrapnum(result);
}

/* returns the square root of x */
value_t std_sqrt(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	sylt_num_t result =
		num_func(sqrtf, sqrt)(numarg(0));
	return wrapnum(result);
}

/* returns the smaller value of a and b */
value_t std_min(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_NUM);
	return wrapnum((numarg(0) < numarg(1))
			? numarg(0)
			: numarg(1));
}

/* returns the larger value of a and b */
value_t std_max(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_NUM);
	return wrapnum((numarg(0) > numarg(1))
			? numarg(0)
			: numarg(1));
}

/* clamps x between lo and hi */
value_t std_clamp(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_NUM);
	typecheck(ctx, arg(2), TYPE_NUM);
	
	sylt_num_t x = numarg(0);
	sylt_num_t lo = numarg(1);
	sylt_num_t hi = numarg(2);
	
	if (x < lo)
		return wrapnum(lo);
	if (x > hi)
		return wrapnum(hi);
	return wrapnum(x);
}

/* returns n rounded towards -infinity */
value_t std_floor(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(floorf, floor)(numarg(0)));
}

/* returns n rounded towards +infinity */
value_t std_ceil(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(ceilf, ceil)(numarg(0)));
}

/* returns the nearest integer value to x, 
 * rounding halfway cases away from zero */
value_t std_round(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(roundf, round)(numarg(0)));
}

/* returns x converted from degrees to
 * radians */
value_t std_rad(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(numarg(0) * M_PI / 180.0);
}

/* returns x converted from radians to
 * degrees */
value_t std_deg(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(numarg(0) * 180.0 / M_PI);
}

/* returns the sine of x */
value_t std_sin(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(sinf, sin)(numarg(0)));
}

/* returns the cosine of x */
value_t std_cos(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return wrapnum(
		num_func(cosf, cos)(numarg(0)));
}

/* returns the tangent of x */
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
	std_addf(ctx, "rep", std_rep, 2);
	
	/* list */
	std_addf(ctx, "length", std_length, 1);
	std_addf(ctx, "push", std_push, 2);
	std_addf(ctx, "pop", std_pop, 1);
	
	/* string */
	std_addf(ctx, "chars", std_chars, 1);
	
	/* math */
	std_add(ctx, "PI", wrapnum(M_PI));
	std_add(ctx, "E", wrapnum(M_E));
	std_addf(ctx, "nearly", std_nearly, 3);
	std_addf(ctx, "signum", std_signum, 1);
	std_addf(ctx, "abs", std_abs, 1);
	std_addf(ctx, "log", std_log, 2);
	std_addf(ctx, "raise", std_raise, 2);
	std_addf(ctx, "sqrt", std_sqrt, 1);
	std_addf(ctx, "min", std_min, 2);
	std_addf(ctx, "max", std_max, 2);
	std_addf(ctx, "clamp", std_clamp, 3);
	std_addf(ctx, "floor", std_floor, 1);
	std_addf(ctx, "ceil", std_ceil, 1);
	std_addf(ctx, "round", std_round, 1);
	std_addf(ctx, "rad", std_rad, 1);
	std_addf(ctx, "deg", std_deg, 1);
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
	T_LESS_MINUS,
	T_GREATER,
	T_GREATER_EQ,
	T_EQ,
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
	/* lexeme */
	string_t* lex;
	/* line number */
	uint32_t line;
} token_t;

/* operator precedence levels,
 * from lowest to highest */
typedef enum {
	PREC_NONE,
	/* assignment '<-' */
	PREC_ASSIGN,
	/* boolean 'or' */
	PREC_OR,
	/* boolean 'and' */
	PREC_AND,
	/* equality: = != */
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
	/* symbol name */
	string_t* name;
	/* scope depth */
	int depth;
	/* captured by a closure */
	bool capped;
} symbol_t;

typedef struct {
	bool is_local;
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
	symbol_t* syms;
	size_t nsyms;
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
	cmp->parent = parent;
	cmp->child = child;
	cmp->func = NULL;
	cmp->curslots = 0;
	
	cmp->src = NULL;
	cmp->pos = NULL;
	cmp->line = 0;
	cmp->syms = NULL;
	cmp->nsyms = 0;
	cmp->depth = 0;
	cmp->ctx = ctx;
}

void comp_free(comp_t* cmp) {
	arr_free(
		cmp->syms,
		symbol_t,
		cmp->nsyms,
		cmp->ctx);
}

void comp_load(comp_t* cmp) {
	string_t* name =
		sylt_peekstring(cmp->ctx, 0);
	string_t* src =
		sylt_peekstring(cmp->ctx, 1);
	
	cmp->func = func_new(cmp->ctx, name);
	if (cmp->parent) {
		/* parent_name/ */
		sylt_pushstring(cmp->ctx,
			cmp->parent->func->path);
		sylt_pushstring(cmp->ctx,
			string_lit("/", cmp->ctx));
		sylt_concat(cmp->ctx);
		
		/* parent_name/child_name */
		sylt_pushstring(cmp->ctx, name);
		sylt_concat(cmp->ctx);
		
		cmp->func->path =
			sylt_popstring(cmp->ctx);
	}
	
	cmp->src = src;
	cmp->pos = (char*)cmp->src->bytes;
	cmp->line = 1;
	
	/* src and name */
	sylt_shrink(cmp->ctx, 2);
}

void comp_copy_parse_state(
	comp_t* dst, const comp_t* src)
{
	dst->pos = src->pos;
	dst->line = src->line;
	dst->prev = src->prev;
	dst->cur = src->cur;
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
int add_symbol(comp_t* cmp, string_t* name) {
	#if DBG_PRINT_NAMES
	for (int i = 0; i < cmp->depth; i++)
		sylt_dprintf("  ");
	
	sylt_dprintf("(sym) ");
	string_dprint(name);
	sylt_dprintf("\n");
	#endif
	
	symbol_t sym;
	sym.name = name;
	sym.depth = cmp->depth;
	sym.capped = false;
	
	cmp->syms = arr_resize(
		cmp->syms,
		symbol_t,
		cmp->nsyms,
		cmp->nsyms + 1,
		cmp->ctx);
	
	cmp->syms[cmp->nsyms++] = sym;
	return cmp->nsyms - 1;
}

/* returns the index of a local variable
 * in the symbol table or -1 if not found */
int find_symbol(comp_t* cmp, string_t* name) {
	if (cmp->nsyms == 0)
		return -1;
	
	/* search backwards in case of
	 * shadowed variable names */
	int start = cmp->nsyms - 1;
	for (int i = start; i >= 0; i--) {
		string_t* other = cmp->syms[i].name;
		if (string_eq(name, other))
			return i;
	}
	
	return -1;
}

/* adds an upvalue to the upvalue array
 * and returns its index */
int add_upvalue(
	comp_t* cmp, uint8_t index, bool is_local)
{
	size_t n = cmp->func->upvalues;
	
	/* check if one already exists */
	for (size_t i = 0; i < n; i++) {
		cmp_upvalue_t upval = cmp->upvals[i];
		if (upval.index == index
			&& upval.is_local == is_local)
			return i;
		
	}
	
	if (n == MAX_UPVALUES) {
		halt(cmp->ctx, E_TOOMANYUPVALUES);
		unreachable();
	}
	
	cmp->upvals[n].is_local = is_local;
	cmp->upvals[n].index = index;
	return cmp->func->upvalues++;
}

/* returns the index of an upvalue in the
 * upvalue array or -1 if not found */
int find_upvalue(
	comp_t* cmp, string_t* name)
{
	if (!cmp->parent)
		return -1;
	
	/* search for a local variable in the
	 * enclosing function */
	int local =
		find_symbol(cmp->parent, name);
	if (local != -1) {
		cmp->parent->syms[local].capped =
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

void comp_open_scope(comp_t* cmp) {
	cmp->depth++;
}

void comp_close_scope(comp_t* cmp) {
	assert(cmp->depth > 0);
	cmp->depth--;
	 
	/* count the number of local variables */
	int locals = 0;
	int index = cmp->nsyms - 1;
	while (index >= 0 &&
		cmp->syms[index].depth > cmp->depth)
	{
		locals++;
		index--;
	}
	
	if (locals == 0)
		return;
		
	/* the return value is on top of the
	 * stack so we need to hide it */
	emit_nullary(cmp, OP_STORE_RET);
	
	/* pop the locals */
	while (cmp->nsyms > 0 &&
		cmp->syms[cmp->nsyms - 1].depth
			> cmp->depth)
	{
		symbol_t* name =
			&cmp->syms[cmp->nsyms - 1];
		cmp->nsyms--;
		
		if (name->capped)
			emit_nullary(cmp, OP_POP_HEAP);
		else
			emit_nullary(cmp, OP_POP);
	}
	
	/* shrink symbol table */
	cmp->syms = arr_resize(
		cmp->syms,
		symbol_t,
		cmp->nsyms + locals,
		cmp->nsyms,
		cmp->ctx);
		
	/* restore the return value */
	emit_nullary(cmp, OP_LOAD_RET);
}

/* == lexer == */

#define token(tag) \
	(token_t){ \
		tag, \
		string_new( \
			(uint8_t*)start, \
			cmp->pos - start, \
			cmp->ctx), \
		cmp->line}
#define step() cmp->pos++
#define peek() (*cmp->pos)
#define peek_next() \
	(eof() ? '\0' : cmp->pos[1])
#define is(c) (peek() == (c))
#define next_is(c) (peek_next() == (c))
#define eof() \
	((uint8_t*)cmp->pos - cmp->src->bytes \
		>= cmp->src->len)
#define match(c) \
	((!eof() && is(c)) ? \
		step(), true : false)

/* scans the source code for the next token */
token_t scan(comp_t* cmp) {
	/* skip over any whitespace */
	while (!eof()) {
		if (!isspace(peek())) {
			/* single-line comment */
			if (is('#') && next_is('#')) {
				while (!is('\n') && !eof())
					step();
				
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
			? (match('-'))
				? T_LESS_MINUS
				: T_LESS
			: T_LESS_EQ);
	case '>':
		return token((!match('='))
			? T_GREATER
			: T_GREATER_EQ);
	case '=': return token(T_EQ);
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
	case '\0': return token(T_EOF);
	}
	
	cmp->prev.line = cmp->line; /* hack */
	halt(cmp->ctx,
		E_UNEXPECTEDCHAR(cmp->pos[-1]));
	return token(-1);
}

#undef token
#undef step
#undef peek
#undef peek_next
#undef is
#undef next_is
#undef eof
#undef match

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
	[T_MINUS_GREATER] = 
		{NULL, NULL, PREC_NONE},
	[T_STAR] = {NULL, binary, PREC_FACTOR},
	[T_SLASH] = {NULL, binary, PREC_FACTOR},
	[T_PERCENT] = {NULL, binary, PREC_FACTOR},
	[T_LESS] = {NULL, binary, PREC_CMP},
	[T_LESS_EQ] = {NULL, binary, PREC_CMP},
	[T_LESS_MINUS] = {NULL, NULL, PREC_NONE},
	[T_GREATER] = {NULL, binary, PREC_CMP},
	[T_GREATER_EQ] = {NULL, binary, PREC_CMP},
	[T_EQ] = {NULL, binary, PREC_EQ},
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
			(int)cmp->prev.lex->len,
			cmp->prev.lex->bytes);
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
	string_t* name = cmp->prev.lex;
	
	bool assign = false;
	if (match(cmp, T_LESS_MINUS)) {
		expr(cmp, PREC_ASSIGN);
		assign = true;
	}
	
	/* check if the name is in this functions
	 * symbol table */
	int index = find_symbol(cmp, name);
	if (index != -1) {
		if (assign)
			emit_unary(cmp, OP_STORE, index);
		else
			emit_unary(cmp, OP_LOAD, index);
		return;
	}
	
	/* check if the name is in an upvalue */
	index = find_upvalue(cmp, name);
	if (index != -1) {
		if (assign)
			emit_unary(cmp,
				OP_STORE_UPVAL, index);
		else
			emit_unary(cmp,
				OP_LOAD_UPVAL, index);
		return;
	}
	
	halt(cmp->ctx, E_UNDEFINED(name));
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
	assert(token.lex->len >= 2); /* "" */
	
	/* allocate an empty string the same size
	 * as the length of the string literal */
	string_t* dst = string_new(
		NULL,
		token.lex->len - 2,
		cmp->ctx);
	size_t write = 0;
	
	const uint8_t* src = token.lex->bytes;
	size_t end = token.lex->len - 1;
	
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
		((char*)cmp->prev.lex->bytes, NULL);
	
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
	case T_EQ: opcode = OP_EQ; break;
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
	
	if (match(cmp, T_LESS_MINUS)) {
		expr(cmp, PREC_ASSIGN);
		emit_nullary(cmp, OP_STORE_LIST);
		return;
	}
	
	emit_nullary(cmp, OP_LOAD_LIST);
}

void parse_func(comp_t*, string_t*);

/* parses a variable or function binding */
void let(comp_t* cmp) {
	dbg_printfname();
	
	eat(cmp, T_NAME,
		"expected variable name after 'let'");
	string_t* name = cmp->prev.lex;
	
	if (match(cmp, T_LPAREN)) {
		 /* parse a function declaration
 		 * in the form of
 		 * let name(p1, p2, ..) = body */
 
		 /* add symbol first in order
	 	 * to support recursion */
		 add_symbol(cmp, name);
		 parse_func(cmp, name);
		
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
	parse_func(cmp, NULL);
}

void parse_func(
	comp_t* cmp, string_t* name)
{
	dbg_printfname();
	
	bool is_lambda = false;
	if (!name) {
		name = string_lit("_", cmp->ctx);
		is_lambda = true;
	}
		
	sylt_pushstring(cmp->ctx, cmp->src);
	sylt_pushstring(cmp->ctx, name);
	
	/* setup a new compiler instance
	* in order to parse the function */
	comp_t fcmp;
	comp_init(&fcmp, cmp, NULL, cmp->ctx);
	comp_load(&fcmp);
	comp_copy_parse_state(&fcmp, cmp);
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
		add_symbol(&fcmp, fcmp.prev.lex);
		fcmp.func->params++;
			
		if (!match(&fcmp, T_COMMA))
			break;
	}
		
	eat(&fcmp, T_RPAREN,
		"expected ')' or a parameter name");
	
	if (is_lambda)
		eat(&fcmp, T_MINUS_GREATER,
			"expected '->' after ')'");
	else
		eat(&fcmp, T_EQ,
			"expected '=' after ')'");
			
	/* function body */
	expr(&fcmp, ANY_PREC);
	
	emit_nullary(&fcmp, OP_RET);
	func_t* func = fcmp.func;
	emit_value(cmp, wrapfunc(func));
	
	/* write arguments to OP_PUSHFUNC */
	for (size_t i = 0;
		i < func->upvalues; i++)
	{
		cmp_upvalue_t* upval =
			&fcmp.upvals[i];
			
		func_write(cmp->func,
			upval->is_local,
			cmp->prev.line,
			cmp->ctx);
		func_write(cmp->func,
			upval->index,
			cmp->prev.line,
			cmp->ctx);
	}
	
	comp_copy_parse_state(cmp, &fcmp);
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
	comp_open_scope(cmp);
	
	/* empty block yields nil */
	if (match(cmp, T_RCURLY)) {
		emit_value(cmp, wrapnil());
		comp_close_scope(cmp);
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
	comp_close_scope(cmp);
}

void std_add(
	sylt_t* ctx,
	const char* name_lit,
	value_t val)
{
	gc_pause(ctx);
	string_t* name = string_lit(
		name_lit, ctx);
	add_symbol(ctx->cmp, name);
	
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
	ctx->mem.gc.pause_depth++;
}

void gc_resume(sylt_t* ctx) {
	if (ctx->mem.gc.state != GC_STATE_PAUSED)
		return;
		
	if (ctx->mem.gc.pause_depth > 0)
		ctx->mem.gc.pause_depth--;
	
	if (ctx->mem.gc.pause_depth == 0)
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
	assert(ctx->mem.gc.state
		== GC_STATE_IDLE);

	gc_mark(ctx, func_name, line);
	gc_sweep(ctx);
	gc_set_state(ctx, GC_STATE_IDLE);
	
	ctx->mem.gc.trigger = ctx->mem.bytes *
		GC_THRESHOLD_CLIMB;
	ctx->mem.gc.cycles++;
}

void gc_mark_vm(sylt_t*);
void gc_mark_compiler(sylt_t*);
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
	gc_mark_vm(ctx);
	gc_mark_compiler(ctx);
	gc_trace_refs(ctx);
	
	gc_free(ctx->mem.gc.marked);
	ctx->mem.gc.marked = NULL;
	ctx->mem.gc.nmarked = 0;
}

/* marks all root objects reachable from
 * the VM */
void gc_mark_vm(sylt_t* ctx) {
	vm_t* vm = ctx->vm;
	if (!vm)
		return;
	
	/* stack */
	value_t* v = vm->stack;
	for (; v < vm->sp; v++)
		val_mark(*v, ctx);
		
	/* call stack */
	for (size_t i = 0; i < vm->nframes; i++)
		obj_mark((obj_t*)vm->frames[i].cls,
			ctx);
	
	/* open upvalues */
	upvalue_t* upval = vm->openups;
	for (; upval; upval = upval->next)
		obj_mark((obj_t*)upval, ctx);
	
	/* return value */
	val_mark(vm->hidden, ctx);
}

/* marks all roots reachable from
 * the compiler */
void gc_mark_compiler(sylt_t* ctx) {
	if (ctx->state != SYLT_STATE_COMPILING)
		return;
	
	/* compiler stack */
	comp_t* cmp = ctx->cmp;
	do {
		obj_mark((obj_t*)cmp->func, ctx);
		obj_mark((obj_t*)cmp->src, ctx);
		obj_mark((obj_t*)cmp->prev.lex, ctx);
		obj_mark((obj_t*)cmp->cur.lex, ctx);
		for (int i = 0; i < cmp->nsyms; i++)
			obj_mark(
				(obj_t*)cmp->syms[i].name,
				ctx);
		
		cmp = cmp->child;
	} while (cmp);
}

/* iterates through all marked roots
 * and marks any child objects reachable
 * from them */
void gc_trace_refs(sylt_t* ctx) {
	while (ctx->mem.gc.nmarked > 0) {
		obj_t* obj = ctx->mem.gc.marked[
			--ctx->mem.gc.nmarked];
		obj_deep_mark(obj, ctx);
	}
}

size_t dbg_count_unreachable(sylt_t* ctx) {
	size_t n = 0;
	obj_t* obj = ctx->mem.objs;
	while (obj) {
		if (obj->marked) {
			obj = obj->next;
			continue;
		}
		
		obj = obj->next;
		n++;
	}
	return n;
}

/* frees the set of unreachable objects */
void gc_sweep(sylt_t* ctx) {
	gc_set_state(ctx, GC_STATE_SWEEP);
	
	#if DBG_PRINT_GC_STATE
	sylt_dprintf("Freeing %ld\n",
		dbg_count_unreachable());
	#endif
	
	obj_t* prev = NULL;
	obj_t* obj = ctx->mem.objs;
	
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
		NULL, len, ctx);
	fread(str->bytes, 1, len, fp);
	fclose(fp);
	
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
	
	/* print message prefix */
	int state = (ctx) ? ctx->state : -1;
	switch (state) {
	case SYLT_STATE_COMPILING: {
		/* find the deepest compiler */
		comp_t* cmp = ctx->cmp;
		while (cmp->child)
			cmp = cmp->child;
		
		sylt_eprintf("error in ");
		string_eprint(cmp->func->path);
		sylt_eprintf(":%d: ", cmp->prev.line);
		break;
	}
	case SYLT_STATE_EXEC: {
		const func_t* func =
			ctx->vm->fp->func;
		
		size_t addr =
			(size_t)(ctx->vm->fp->ip -
				func->code - 1);
		uint32_t line = func->lines[addr];
		
		sylt_eprintf("error in ");
		string_eprint(func->path);
		sylt_eprintf(":%d: ", line);
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
	comp_init(ctx->cmp, NULL, NULL, ctx);
	
	/* init memory */
	ctx->mem.objs = NULL;
	/* has to be done manually when
	 * allocating ctx struct itself */
	ctx->mem.bytes += sizeof(sylt_t);
	ctx->mem.highest = 0;
	ctx->mem.count = 0;
	ctx->mem.objcount = 0;
	
	/* init GC state */
	ctx->mem.gc.marked = NULL;
	ctx->mem.gc.nmarked = 0;
	ctx->mem.gc.trigger = GC_INIT_THRESHOLD;
	ctx->mem.gc.cycles = 0;
	ctx->mem.gc.pause_depth = 0;
	
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
		comp_t* cmp = ctx->cmp;
		do {
			comp_t* next = cmp->child;
			comp_free(cmp);
			cmp = next;
		} while (cmp);
		
		ptr_free(ctx->cmp, comp_t, ctx);
		ctx->cmp = NULL;
	}
	
	dbg_print_mem_stats(ctx);
	assert(ctx->mem.gc.pause_depth == 0);
	
	/* ensure that no memory was leaked */
	ptrdiff_t bytesleft =
		ctx->mem.bytes - sizeof(sylt_t);
	if (bytesleft)
		sylt_dprintf("%ld bytes leaked\n",
			bytesleft);
	assert(!bytesleft);
	
	sylt_set_state(ctx, SYLT_STATE_FREE);
	ptr_free(ctx, sylt_t, ctx);
}

void compile(sylt_t* ctx) {
	gc_pause(ctx);
	
	comp_load(ctx->cmp);
	
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
	
	gc_resume(ctx);
}

/* compiles and runs a sylt program from
 * a string, returning true if successful */
bool sylt_dostring(
	sylt_t* ctx, const char* src)
{
	if (!ctx || !src)
		return false;
		
	if (setjmp(err_jump)) {
		gc_resume(ctx);
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
	vm_load(ctx->vm);
	vm_exec(ctx->vm, false);
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
		gc_resume(ctx);
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
	vm_load(ctx->vm);
	vm_exec(ctx->vm, false);
	return true;
}

void sylt_interact(sylt_t* ctx) {
	puts(SYLT_VERSION_STR);
	while (true) {
		printf(">> ");
		char buffer[4096];
		fgets(buffer, 4096, stdin);
		sylt_dostring(ctx, buffer);
	}
}

void sylt_test(sylt_t* ctx) {
	/* empty input */
	assert(sylt_dostring(ctx, ""));
	
	/* make sure errors don't
	 * cause any problems */
	test_errors(ctx);
	
	/* make sure ensure(false) halts
	 * (used all over in tests.sylt) */
	assert(!sylt_dostring(
		ctx, "ensure(false)"));
	
	/* run main tests */
	sylt_dprintf("Running tests:\n");
	sylt_dofile(ctx, "tests.sl");
}

int main(int argc, char *argv[]) {
	dbg_print_flags();
	
	sylt_t* ctx = sylt_new();
	sylt_test(ctx);
	
	/*if (argc == 1)
		sylt_interact(ctx);
	else
		sylt_dofile(ctx, argv[1]);*/
	
	sylt_free(ctx);
	
	return EXIT_SUCCESS;
}
