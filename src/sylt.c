#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <setjmp.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <dirent.h> 

#define SYLT_VERSION_STR "sylt 0.1"
#define SYLT_VERSION_MAJ 0
#define SYLT_VERSION_MIN 1
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

/* initial stack size, more will be
 * automatically allocated as needed */
#define SYLT_INIT_STACK 512

/* some extra stack space for hiding stuff
 * from the GC (temporary) */
#define SYLT_EXTRA_STACK 32

/* path to standard library relative to
 * executable */
#define SYLT_STDLIB_PATH "src/stdlib.sylt"

/* number of bytes that need to be allocated
 * before triggering the first GC cycle
 * (0.25 MB by default) */
#define GC_INIT_THRESHOLD (1024 * 1024 * 0.25)

/* how much to multiply the GC threshold
 * by after each collection cycle */
#define GC_THRESHOLD_CLIMB 1.5

/* == debug flags ==
 * all of these should be set to 0 in
 * release builds */

#if DEBUG

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
#define DBG_PRINT_DATA 0
#define DBG_PRINT_STACK 0

#else

#define DBG_ASSERTIONS 0
#define DBG_NO_GC 0
#define DBG_GC_EVERY_ALLOC 0
#define DBG_PRINT_SYLT_STATE 0
#define DBG_PRINT_GC_STATE 0
#define DBG_PRINT_DATA 0
#define DBG_PRINT_STACK 0

#endif

/* prints all debug flags that are set */
static void dbg_print_flags(void) {
	#define pflag(flag) \
		if (flag) \
			sylt_dprintf("%s enabled\n", #flag)
			
	pflag(DBG_ASSERTIONS);
	pflag(DBG_NO_GC);
	pflag(DBG_GC_EVERY_ALLOC);
	pflag(DBG_PRINT_SYLT_STATE);
	pflag(DBG_PRINT_GC_STATE);
	pflag(DBG_PRINT_DATA);
	pflag(DBG_PRINT_STACK);
			
	#undef pflag
}

/* == constant limits == */

#define MAX_CODE (UINT16_MAX + 1)
#define MAX_DATA (UINT8_MAX + 1)
#define MAX_STACK (UINT8_MAX + 1)
#define MAX_LINES UINT32_MAX
#define MAX_JUMP UINT16_MAX
#define MAX_CFRAMES 32768
#define MAX_PARAMS UINT8_MAX
#define MAX_UPVALUES UINT8_MAX
#define MAX_ERRMSGLEN 2048
#define MAX_FILES 2048

/* == debug macros == */

#if DBG_ASSERTIONS
#define dbgerr(msg) \
	sylt_dprintf("%s in %s:%d", (msg), __FILE__, __LINE__); \
	exit(EXIT_FAILURE);
#define assert(cond) \
	if (!(cond)) { \
		dbgerr("assertion failed"); \
	}
#define unreachable() \
	dbgerr("unreachable code entered");
#else
#define dbgerr(msg)
#define assert(cond)
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

/* garbage collector */
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

/* memory */
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
	struct vm_s* vm;
	struct comp_s* cmp;
	mem_t mem;
	bool disassemble;
} sylt_t;

/* longjmp destination on error.
 * kept outside sylt_t struct as an
 * out of memory error could occur
 * while allocating it */
static jmp_buf err_jump;

static void set_state(sylt_t* ctx, sylt_state_t state) {
	ctx->state = state;
	#if DBG_PRINT_SYLT_STATE
	sylt_dprintf("[%p: %s]\n", ctx, SYLT_STATE_NAME[state]);
	#endif
}

/* == API == */

sylt_t* sylt_new(void);
void sylt_free(sylt_t* ctx);
bool sylt_xstring(sylt_t* ctx, const char* src);
bool sylt_xfile(sylt_t* ctx, const char* path);
	
/* == error messages == */

void halt(sylt_t*, const char*, ...);

#define E_OUT_OF_MEM \
	"out of memory"
#define E_CODE_LIMIT \
	"bytecode limit of %d reached", MAX_CODE
#define E_DATA_LIMIT \
	"data limit of %d reached", MAX_DATA
#define E_STACK_LIMIT \
	"stack limit of %d reached", MAX_STACK
#define E_LINE_LIMIT \
	"line limit of %d reached", MAX_LINES
#define E_JUMP_LIMIT \
	"jump distance too far (>%d)", MAX_JUMP
#define E_OPEN_FAILED(path) \
	"failed to open file '%s'", (path)
#define E_INVALID_HANDLE(handle) \
	"invalid file handle: %d", (handle)
#define E_UNEXPECTED_CHAR(c) \
	"unexpected character '%c'", (c)
#define E_PARSER_EXPECTED(msg, got) \
	"%s, got '%.*s'", msg, (int)got->len, got->bytes
#define E_TYPE(ex, got) \
	"expected %s, got %s", user_type_name(ex), user_type_name(got)
#define E_ARG_TYPE(ex, got, n, fname) \
	"expected %s for argument #%d to %s, got %s", \
	user_type_name(ex), (n) + 1, (fname), user_type_name(got)
#define E_NOT_CALLABLE(tag) \
	"cannot call a value of type %s", user_type_name(tag)
#define E_NO_LENGTH(tag) \
	"cannot get the length of a variable of type %s", user_type_name(tag)
#define E_CONCAT_TYPE(a, b) \
	"cannot concatenate a %s with a %s", user_type_name(a), user_type_name(b)
#define E_ESCAPE_SEQ(code) \
	"unknown escape sequence '\\%c'", (code)
#define E_UNTERM_STRING \
	"unterminated string literal"
#define E_TOO_MANY_PARAMS \
	"too many parameters; limit is %d", (MAX_PARAMS)
#define E_TOO_MANY_ARGS \
	"too many arguments; limit is %d", (MAX_PARAMS)
#define E_TOO_MANY_UPVALUES \
	"too many upvalues; max is %d", (MAX_UPVALUES)
#define E_UNDEFINED(name) \
	"undefined variable '%.*s'", (name->len), (name->bytes)
#define E_KEY_NOT_FOUND(key) \
	"key '%.*s' not found in dictionary", (key->len), (key->bytes)
#define E_DIV_BY_ZERO \
	"attempted division by zero"
#define E_STACK_OVERFLOW \
	"call stack overflow"
#define E_INDEX(len, index) \
	"index out of range, len: %d, i=%d", (len), (index)
#define E_WRONG_ARGC(name, need, got) \
	"%.*s takes %d argument%s but got %d", \
	(int)name->len, name->bytes, \
	(need), ((need) == 1 ? "" : "s"), (got)
#define E_ENSURE_FAILED \
	"assertion failed"
#define E_TODO_REACHED \
	"code marked todo was reached"
#define E_UNREACHABLE_REACHED \
	"code marked unreachable was reached"
#define E_SYLT_EXTENSION \
	"the .sylt extension is not required"

/* triggers one of each error */
void test_errors(sylt_t* ctx) {
	#if DEBUG
	#define testsrc() \
		sylt_dprintf("(expected) "); \
		assert(!sylt_xstring(ctx, src));
	
	const char* src;
	
	/* unexpected character */
	src = "`";
	testsrc();
	
	/* type error */
	src = "1 + true";
	testsrc();
	
	/* unknown escape sequence */
	src = "\" \\w \"";
	testsrc();
	
	/* unterminated string literal */
	src = "\"";
	testsrc();
	
	/* undefined variable */
	src = "1 + milk";
	testsrc();
	
	/* division by zero */
	src = "1 / 0";
	testsrc();
	
	/* stack overflow */
	src =
		"let loop(i, n) = "
		"	if (i < n) "
		"		loop(i + 1, n) "
		"loop(0, 32769)";
	testsrc();
	
	/* index */
	src =
		"let list = []"
		"list[0]";
	testsrc();
	
	/* wrong argument count */
	src = "ensure(1, 2)";
	testsrc();
	
	/* ensure(false) */
	src = "ensure(false)";
	testsrc();
	
	/* todo() */
	src = "todo()";
	testsrc();
	
	/* unreachable() */
	src = "unreachable()";
	testsrc();
	#else
	(void)ctx;
	#endif
}

static const char* get_platform(void) {
	#ifdef _WIN32
    return "Windows 32-bit";
    #elif _WIN64
    return "Windows 64-bit";
    #elif __APPLE__ || __MACH__
    return "Mac OSX";
    #elif __linux__
    return "Linux";
    #elif __FreeBSD__
    return "FreeBSD";
    #elif __unix || __unix__
    return "Unix";
    #else
    return "Unknown";
    #endif
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
	if (ns == os)
		return p;
	
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
		if (ctx->mem.bytes > (ptrdiff_t)ctx->mem.gc.trigger)
			gc_collect(ctx, func_name, line);
		#endif
	}
	
	#if DBG_PRINT_GC_STATE
	if (ns > 0)
		sylt_dprintf("        %+ld to %s in %s:%d\n",
			ns - os, p_name, func_name, line);
	#else
	(void)p_name;
	#endif
	
	if (ns == 0) {
		free(p);
		return NULL;
	}
	
	void* np = realloc(p, ns);
	if (!np) {
		halt(ctx, E_OUT_OF_MEM);
	    unreachable();
	}
	
	return np;
}

/* useful macros for allocating pointers */
#define ptr_alloc(p, t, ctx) \
	((p) = ptr_resize(NULL, 0, sizeof(t), #p, __func__, __LINE__, ctx))
	
#define ptr_free(p, t, ctx) \
	ptr_resize(p, sizeof(t), 0, #p, __func__, __LINE__, ctx)

/* macros for handling dynamic arrays */
#define arr_alloc(p, t, n, ctx) \
	((p) = ptr_resize(NULL, 0, sizeof(t) * n, #p, __func__, __LINE__, ctx))

#define arr_resize(p, t, os, ns, ctx) \
	ptr_resize(p, sizeof(t) * (os), sizeof(t) * (ns), #p, __func__, __LINE__, ctx)

#define arr_free(p, t, os, ctx) \
	ptr_resize(p, sizeof(t) * (os), 0, #p, __func__, __LINE__, ctx)

/* == opcodes == */

typedef enum {
	/* stack */
	OP_PUSH,
	OP_PUSH_NULL,
	OP_PUSH_TRUE,
	OP_PUSH_FALSE,
	OP_PUSH_LIST,
	OP_PUSH_DICT,
	OP_PUSH_FUNC,
	OP_POP,
	OP_POP_HEAP,
	OP_DUP,
	/* memory */
	OP_LOAD,
	OP_STORE,
	OP_ADD_NAME,
    OP_ADD_MOD,
	OP_LOAD_NAME,
	OP_STORE_NAME,
	OP_LOAD_KEY,
	OP_STORE_KEY,
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
	OP_LENGTH,
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
	OP_JMP_IF,
	OP_JMP_IF_NOT,
	OP_JMP_BACK,
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
	[OP_PUSH_NULL] = {"pushNull", 0, +1},
	[OP_PUSH_TRUE] = {"pushTrue", 0, +1},
	[OP_PUSH_FALSE] = {"pushFalse", 0, +1},
	[OP_PUSH_LIST] = {"pushList", 1, +1},
	[OP_PUSH_DICT] = {"pushDict", 1, +1},
	[OP_PUSH_FUNC] = {"pushFunc", 1, +1},
	[OP_POP] = {"pop", 0, -1},
	[OP_POP_HEAP] = {"popHeap", 0, -1},
	[OP_DUP] = {"dup", 0, +1},
	[OP_LOAD] = {"load", 1, +1},
	[OP_STORE] = {"store", 1, 0},
	[OP_ADD_NAME] = {"addName", 1, -1},
    [OP_ADD_MOD] = {"addMod", 2, -1},
	[OP_LOAD_NAME] = {"loadName", 1, +1},
	[OP_STORE_NAME] = {"storeName", 1, 0},
	[OP_LOAD_KEY] = {"loadKey", 0, -1},
	[OP_STORE_KEY] = {"storeKey", 0, -2},
	[OP_LOAD_UPVAL] = {"loadUpval", 1, +1},
	[OP_STORE_UPVAL] = {"storeUpval", 1, 0},
	[OP_LOAD_RET] = {"loadRet", 0, +1},
	[OP_STORE_RET] = {"storeRet", 0, -1},
	[OP_ADD] = {"add", 0, -1},
	[OP_SUB] = {"sub", 0, -1},
	[OP_MUL] = {"mul", 0, -1},
	[OP_DIV] = {"div", 0, -1},
	[OP_EDIV] = {"ediv", 0, -1},
	[OP_UMIN] = {"umin", 0, 0},
	[OP_LENGTH] = {"length", 0, 0},
	[OP_LT] = {"lt", 0, -1},
	[OP_LTE] = {"lte", 0, -1},
	[OP_GT] = {"gt", 0, -1},
	[OP_GTE] = {"gte", 0, -1},
	[OP_EQ] = {"eq", 0, -1},
	[OP_NEQ] = {"neq", 0, -1},
	[OP_NOT] = {"not", 0, 0},
	[OP_JMP] = {"jmp", 2, 0},
	[OP_JMP_IF] = {"jmpIf", 2, 0},
	[OP_JMP_IF_NOT] = {"jmpIfNot", 2, 0},
	[OP_JMP_BACK] = {"jmpBack", 2, 0},
	[OP_CALL] = {"call", 1, 0},
	[OP_RET] = {"ret", 0, 0},
};

/* value types */
typedef enum {
	TYPE_UNIT,
	TYPE_BOOL,
	TYPE_NUM,
	TYPE_LIST,
	TYPE_DICT,
	TYPE_STRING,
	TYPE_FUNCTION,
	TYPE_CLOSURE,
	TYPE_UPVALUE,
} type_t;

static const char* TYPE_NAMES[] = {
	"Null",
	"Bool",
	"Number",
	"List",
	"Dict",
	"String",
	"Function",
	"Closure",
	"Upvalue",
};

static const char* user_type_name(
	type_t tag)
{
	switch (tag) {
	case TYPE_CLOSURE: return TYPE_NAMES[TYPE_FUNCTION];
	case TYPE_FUNCTION:
	case TYPE_UPVALUE: unreachable();
	default: return TYPE_NAMES[tag];
	}
}

#define isheaptype(t) \
	((t) == TYPE_LIST \
		|| (t) == TYPE_DICT \
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

/* dictionary entry */
typedef struct {
	struct string_s* key;
	value_t val;
} item_t;

/* dictionary object */
typedef struct {
	obj_t obj;
	item_t* items;
	size_t len;
	size_t cap;
} dict_t;

/* string object */
typedef struct string_s {
	obj_t obj;
	/* null terminated list of characters */
	uint8_t* bytes;
	size_t len;
	/* for use as a dictionary key */
	uint32_t hash;
} string_t;

/* function pointer to a sylt library
 * function written in C */
typedef struct value_s (*cfunc_t)
	(sylt_t* ctx);

/* function object */
typedef struct {
	obj_t obj;
	
	/* bytecode */
	uint8_t* code;
	size_t ncode;
	/* constant data */
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
	/* source code */
	string_t* src;
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
	
	/* global variables */
	dict_t* gdict;
	
	/* linked list of open upvalues */
	upvalue_t* openups;
	
	/* special slot for return value */
	value_t hidden;
	
	/* file handles opened by user */
	FILE* files[MAX_FILES];
	
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
#define unit() (value_t){TYPE_UNIT, {.num = 0}}
#define wrapbool(v) (value_t){TYPE_BOOL, {.num = (v)}}
#define wrapnum(v) (value_t){TYPE_NUM, {.num = (v)}}
#define wraplist(v) (value_t){TYPE_LIST, {.obj = (obj_t*)(v)}}
#define wrapdict(v) (value_t){TYPE_DICT, {.obj = (obj_t*)(v)}}
#define wrapstring(v) (value_t){TYPE_STRING, {.obj = (obj_t*)(v)}}
#define wrapfunc(v) (value_t){TYPE_FUNCTION, {.obj = (obj_t*)(v)}}
#define wrapclosure(v) (value_t){TYPE_CLOSURE, {.obj = (obj_t*)(v)}}
#define wrapupvalue(v) (value_t){TYPE_UPVALUE, {.obj = (obj_t*)(v)}}

/* macros for getting the raw value from
 * a value_t. the type must be
 * checked before accessing these to
 * prevent undefined behaviour */
#define getbool(v) (v).data.num
#define getnum(v) (v).data.num
#define getobj(v) (v).data.obj
#define getlist(v) ((list_t*)getobj(v))
#define getdict(v) ((dict_t*)getobj(v))
#define getstring(v) ((string_t*)getobj(v))
#define getfunc(v) ((func_t*)getobj(v))
#define getclosure(v) ((closure_t*)getobj(v))
#define getupvalue(v) ((upvalue_t*)getobj(v))

#define typecheck(ctx, v, t) if (v.tag != t) halt(ctx, E_TYPE(t, v.tag))
#define typecheck2(ctx, a, b, t) typecheck(ctx, a, t); typecheck(ctx, b, t)
	
bool val_eq(value_t, value_t);
	
/* == public stack API == */
/* these are useful even outside of the VM,
 * for making objects visible to the GC */

void vm_ensure_stack(vm_t*, size_t);
static inline void vm_push(struct vm_s*, struct value_s);
static inline value_t vm_pop(struct vm_s*);
static inline value_t vm_peek(const struct vm_s*, int);
static inline void vm_shrink(struct vm_s*, int);
	
#define sylt_ensure_stack(ctx, n) \
	vm_ensure_stack(ctx->vm, n)

/* for pushing values on the stack */
#define sylt_push(ctx, v) vm_push(ctx->vm, v)
#define sylt_pushunit(ctx) sylt_push(ctx, unit())
#define sylt_pushbool(ctx, v) sylt_push(ctx, wrapbool(v))
#define sylt_pushnum(ctx, v) sylt_push(ctx, wrapnum(v))
#define sylt_pushlist(ctx, v) sylt_push(ctx, wraplist(v))
#define sylt_pushdict(ctx, v) sylt_push(ctx, wrapdict(v))
#define sylt_pushstring(ctx, v) sylt_push(ctx, wrapstring(v))
#define sylt_pushfunc(ctx, v) sylt_push(ctx, wrapfunc(v))
#define sylt_pushclosure(ctx, v) sylt_push(ctx, wrapclosure(v))

/* for popping values off the stack */
#define sylt_pop(ctx) vm_pop(ctx->vm)
#define sylt_popunit(ctx) sylt_pop(ctx)
#define sylt_popbool(ctx) getbool(sylt_pop(ctx))
#define sylt_popnum(ctx) getnum(sylt_pop(ctx))
#define sylt_poplist(ctx) getlist(sylt_pop(ctx))
#define sylt_popdict(ctx) getdict(sylt_pop(ctx))
#define sylt_popstring(ctx) getstring(sylt_pop(ctx))
#define sylt_popfunc(ctx) getfunc(sylt_pop(ctx))

/* for peeking values down the stack */
#define sylt_peek(ctx, n) vm_peek(ctx->vm, n)
#define sylt_peekbool(ctx, n) getbool(sylt_peek(ctx, n))
#define sylt_peeknum(ctx, n) getnum(sylt_peek(ctx, n))
#define sylt_peeklist(ctx, n) getlist(sylt_peek(ctx, n))
#define sylt_peekdict(ctx, n) getdict(sylt_peek(ctx, n))
#define sylt_peekstring(ctx, n) getstring(sylt_peek(ctx, n))
#define sylt_peekfunc(ctx, n) getfunc(sylt_peek(ctx, n))
#define sylt_peekclosure(ctx, n) getclosure(sylt_peek(ctx, n))

/* shrinks the stack by n values */
#define sylt_shrink(ctx, n) vm_shrink(ctx->vm, n)
	
void dbg_print_obj_mem(obj_t* obj, bool freed) {
	if (!obj)
		return;
	
	sylt_dprintf("        %s%s %p\n",
		(freed) ? "-" : "+",
		TYPE_NAMES[obj->tag],
		obj);
}

/* allocates a generic object */
obj_t* obj_new_impl(
	size_t size, type_t tag, const char* func_name, int line,sylt_t* ctx)
{
	assert(isheaptype(tag));
	obj_t* obj = ptr_resize(
		NULL, 0, size, "<obj>", func_name, line, ctx);
	
	obj->tag = tag;
	obj->marked = false;
	obj->next = ctx->mem.objs;
	ctx->mem.objs = obj;
	ctx->mem.objcount++;
	
	#if DBG_PRINT_GC_STATE
	dbg_print_obj_mem(obj, false);
	#endif
	return obj;
}

#define obj_new(size, tag, ctx) \
	obj_new_impl(size, tag, \
		__func__, __LINE__, ctx);

void list_free(list_t*, sylt_t*);
void dict_free(dict_t*, sylt_t*);
void string_free(string_t*, sylt_t*);
void func_free(func_t*, sylt_t*);
void closure_free(closure_t*, sylt_t*);
void upvalue_free(upvalue_t*, sylt_t*);

/* frees a generic object and its
 * associated memory */
void obj_free(obj_t* obj, sylt_t* ctx) {
	if (!obj)
		return;
		
	assert(isheaptype(obj->tag));
	
	#if DBG_PRINT_GC_STATE
	dbg_print_obj_mem(obj, true);
	#endif
	
	switch (obj->tag) {
	case TYPE_LIST: {
		list_free((list_t*)obj, ctx);
		break;
	}
	case TYPE_STRING: {
		string_free((string_t*)obj, ctx);
		break;
	}
	case TYPE_DICT: {
		dict_free((dict_t*)obj, ctx);
		break;
	}
	case TYPE_FUNCTION: {
		func_free((func_t*)obj, ctx);
		break;
	}
	case TYPE_CLOSURE: {
		closure_free((closure_t*)obj, ctx);
		break;
	}
	case TYPE_UPVALUE: {
		upvalue_free((upvalue_t*)obj, ctx);
		break;
	}
	default: unreachable();
	}
}

/* marks an object as reachable by the
 * garbage collector */
void obj_mark(obj_t* obj, sylt_t* ctx) {
	if (!obj || obj->marked)
		return;
	
	/* set a mark bit on the object itself */
	obj->marked = true;

	/* add the object to a cache for
	 * performance */
	ctx->mem.gc.marked = gc_realloc(
		ctx->mem.gc.marked, (sizeof(obj_t*) * ctx->mem.gc.nmarked) + 1);
	
	if (!ctx->mem.gc.marked) {
		halt(ctx, E_OUT_OF_MEM);
		unreachable();
	}
	
	ctx->mem.gc.marked[ctx->mem.gc.nmarked++] = obj;
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
	case TYPE_DICT: {
		dict_t* dc = (dict_t*)obj;
		for (size_t i = 0; i < dc->cap; i++) {
			item_t* item = &dc->items[i];
			if (!item->key)
				continue;
			obj_mark((obj_t*)item->key, ctx);
			val_mark(item->val, ctx);
		}
		
		break;
	}
	case TYPE_STRING: break;
	case TYPE_FUNCTION: {
		func_t* func = (func_t*)obj;
		for (size_t i = 0; i < func->ndata; i++)
			val_mark(func->data[i], ctx);
			
		obj_mark((obj_t*)func->name, ctx);
		obj_mark((obj_t*)func->path, ctx);
		break;
	}
	case TYPE_CLOSURE: {
		closure_t* cls = (closure_t*)obj;
		obj_mark((obj_t*)cls->func, ctx);
		for (size_t i = 0; i < cls->nupvals; i++)
			obj_mark((obj_t*)cls->upvals[i], ctx);
		
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

list_t* list_new(sylt_t* ctx) {
	list_t* ls = (list_t*)obj_new(sizeof(list_t), TYPE_LIST, ctx);
	ls->items = NULL;
	ls->len = 0;
	return ls;
}

void list_free(list_t* ls, sylt_t* ctx) {
	arr_free(ls->items, value_t, nextpow2(ls->len), ctx);
	ptr_free(ls, list_t, ctx);
}

/* returns true if the two lists are equal */
bool list_eq(const list_t* a, const list_t* b) {
	if (!a || !b) return false;
	if (a == b) return true;
	if (a->len != b->len) return false;
		
	for (size_t i = 0; i < a->len; i++)
		if (!val_eq(a->items[i], b->items[i]))
			return false;
		
	return true;
}

/* converts a possibly negative index to
 * a positive one and halts if it is out
 * of bounds */
size_t list_index(const list_t* ls, int index, sylt_t* ctx) {
	if (index < 0)
		index = ls->len + index;
	
	if (ls->len == 0 ||
		index > (int)ls->len - 1) {
		halt(ctx, E_INDEX(ls->len, index));
		unreachable();
	}
	
	return index;
}

/* returns the nth item in the list, errors
 * if the index is out of bounds */
value_t list_get(list_t* ls, int index, sylt_t* ctx) {
	index = list_index(ls, index, ctx);
	return ls->items[index];
}

/* sets the value at index n, errors if out
 * of bounds */
void list_set(list_t* ls, int index, value_t val, sylt_t* ctx) {
	index = list_index(ls, index, ctx);
	ls->items[index] = val;
}

/* inserts an item at the given index */
void list_insert(list_t* ls, int index, value_t val, sylt_t* ctx) {
	ls->items = arr_resize(
		ls->items,
		value_t,
		nextpow2(ls->len),
		nextpow2(ls->len + 1),
		ctx);
	ls->len++;
	
	index = list_index(ls, index, ctx);
	for (int i = ls->len - 1; i > index; i--)
		ls->items[i] = ls->items[i - 1];
	ls->items[index] = val;
}

/* deletes an item from the given index */
value_t list_delete(list_t* ls, int index, sylt_t* ctx) {
	index = list_index(ls, index, ctx);
	value_t val = ls->items[index];
	for (int i = index; i <
		(int)ls->len - 1; i++)
		ls->items[i] = ls->items[i + 1];
	
	ls->items = arr_resize(
		ls->items,
		value_t,
		nextpow2(ls->len),
		nextpow2(ls->len - 1),
		ctx);
	ls->len--;
	return val;
}

/* appends an item to the end of the list */
void list_push(list_t* ls, value_t val, sylt_t* ctx) {
	list_insert(ls, -1, val, ctx);
}

/* returns and then deletes the last item
 * from the list */
value_t list_pop(list_t* ls, sylt_t* ctx) {
	return list_delete(ls, -1, ctx);
}

/* returns the concatenation of a and b */
list_t* list_concat(const list_t* a, const list_t* b, sylt_t* ctx) {
	gc_pause(ctx);
	list_t* result = list_new(ctx);
	
	for (size_t i = 0; i < a->len; i++)
		list_push(result, a->items[i], ctx);
	
	for (size_t i = 0; i < b->len; i++)
		list_push(result, b->items[i], ctx);
	
	gc_resume(ctx);
	return result;
}

/* counts the occurrences of a unique value in
 * the list */
size_t list_count(const list_t* ls, value_t val) {
	size_t count = 0;
	for (size_t i = 0; i < ls->len; i++) {
		if (val_eq(ls->items[i], val))
			count++;
	}
	return count;
}

/* == dict == */

#define DICT_MAX_LOAD 0.75

dict_t* dict_new(sylt_t* ctx) {
	dict_t* dc = (dict_t*)obj_new(sizeof(dict_t), TYPE_DICT, ctx);
	dc->items = NULL;
	dc->len = 0;
	dc->cap = 0;
	return dc;
}

void dict_free(dict_t* dc, sylt_t* ctx) {
	arr_free(dc->items, item_t, dc->cap, ctx);
	ptr_free(dc, dict_t, ctx);
}

bool string_eq(const string_t* a, const string_t* b);

/* returns true if the two dicts are equal */
bool dict_eq(const dict_t* a, const dict_t* b) {
	if (!a || !b) return false;
	if (a == b) return true;
	if (a->len != b->len) return false;
	if (a->cap != b->cap) return false;
	
	for (size_t i = 0; i < a->cap; i++) {
		if (!string_eq(a->items[i].key, b->items[i].key))
			return false;
		
		if (!val_eq(a->items[i].val, b->items[i].val))
			return false;
	}
		
	return true;
}

/* finds an entry in a dictionary from
 * a string key value */
item_t* dict_find(const string_t* key, const item_t* items, size_t cap) {
	/* key->hash % cap */
	uint32_t index = key->hash & (cap - 1);
	for (;;) {
		const item_t* item = &items[index];
		if (!item->key || string_eq(item->key, key))
			return (item_t*)item;
		
		/* (index + 1) % cap */
		index = (index + 1) & (cap - 1);
	}
}

/* retrieves a value from the dictionary */
value_t* dict_get(const dict_t* dc, const string_t* key) {
	if (dc->len == 0)
		return NULL;
		
	item_t* item = dict_find(key, dc->items, dc->cap);
	if (!item->key)
		return NULL;
	
	return &item->val;
}

/* sets the capacity and reallocates the 
 * backing array */
void dict_set_cap(dict_t* dc, size_t cap, sylt_t* ctx) {
	/* allocate a new array */
	item_t* items = arr_alloc(items, item_t, cap, ctx);
	for (size_t i = 0; i < cap; i++) {
		items[i].key = NULL;
		items[i].val = unit();
	}
	
	/* copy old entries to new array */
	for (size_t i = 0; i < dc->cap; i++) {
		item_t* src = &dc->items[i];
		if (!src->key)
			continue;
			
		item_t* dst = dict_find(src->key, items, cap);
		dst->key = src->key;
		dst->val = src->val;
	}
	
	/* free the original */
	arr_free(dc->items, item_t, dc->cap, ctx);
	
	/* update */
	dc->items = items;
	dc->cap = cap;
}

/* inserts a value into the dictionary */
bool dict_set(dict_t* dc, const string_t* key, value_t val, sylt_t* ctx) {
	/* grow if needed */
	size_t cap = dc->cap * DICT_MAX_LOAD;
	if (dc->len + 1 > cap) {
		int new_cap = nextpow2(dc->cap + 1);
		dict_set_cap(dc, new_cap, ctx);
	}
	
	item_t* item = dict_find(key, dc->items, dc->cap);
	bool is_new = !item->key;
	if (is_new)
		dc->len++;
	
	item->key = (string_t*)key;
	item->val = val;
	return is_new;
}

void dict_copy(dict_t* dst, const dict_t* src, bool overwrite, sylt_t* ctx) {
	for (size_t i = 0; i < src->cap; i++) {
		item_t* item = &src->items[i];
		if (!item->key)
			continue;
		
		bool exists = !overwrite && dict_get(dst, item->key);
		if (exists)
			continue;
		
		dict_set(dst,item->key, item->val, ctx);
	}
}

/* FNV-1a */
uint32_t dict_calc_hash(const uint8_t* bytes, size_t len) {
	uint32_t hash = 2166136261u;
	for (size_t i = 0; i < len; i++) {
		hash ^= bytes[i];
		hash *= 16777619;
	}
	return hash;
}

/* == string == */

void string_rehash(string_t*, sylt_t*);

string_t* string_new(uint8_t* bytes, size_t len, sylt_t* ctx) {
	string_t* str = (string_t*)obj_new(sizeof(string_t), TYPE_STRING, ctx);
	sylt_pushstring(ctx, str); /* GC */
	
	str->bytes = NULL;
	arr_alloc(str->bytes, uint8_t, len + 1, ctx);
	
	if (bytes)
		memcpy(str->bytes, bytes, len);
	
	str->len = len;
	string_rehash(str, ctx);
	return sylt_popstring(ctx);
}

void string_free(string_t* str, sylt_t* ctx) {
	arr_free(str->bytes, uint8_t, str->len + 1, ctx);
	ptr_free(str, dict_t, ctx);
}

/* helper function for creating a new 
 * string object from a C string literal */
string_t* string_lit(const char* lit, sylt_t* ctx) {
	return string_new((uint8_t*)lit, strlen(lit), ctx);
}

/* creates a new formatted string */
string_t* string_fmt(sylt_t* ctx, const char* fmt, ...) {
	uint8_t buffer[2048];
	va_list args;
	va_start(args, fmt);
	vsnprintf((char*)buffer, 2048, fmt, args);
	va_end(args);

	string_t* str = string_new(buffer, strlen((char*)buffer), ctx);
	return str;
	
	/*va_list args;
	va_start(args, fmt);

	size_t len = vsnprintf(NULL, 0, fmt, args);
	string_t* str = string_new(NULL, len, ctx);
	vsnprintf((char*)str->bytes, len, fmt, args);
	string_rehash(str, ctx);

	va_end(args);
	return str;*/
}

/* must be called whenever the contents
 * of a string changes */
void string_rehash(string_t* str, sylt_t* ctx) {
	(void)ctx;
	
	/* add null terminator */
	if (str->bytes[str->len] != '\0')
		str->bytes[str->len] = '\0';
	
	/* hash it */
	str->hash = dict_calc_hash(
		str->bytes, str->len);
}

/* returns true if a == b */
bool string_eq(const string_t* a, const string_t* b) {
	if (!a || !b) return false;
	if (a == b) return true;
	if (a->len != b->len) return false;
	return memcmp(a->bytes, b->bytes, a->len) == 0;
}

/* concatenates two strings */
string_t* string_concat(const string_t* a, const string_t* b, sylt_t* ctx) {
	assert(a && b);
	
	string_t* result = string_new(NULL, a->len + b->len, ctx);
	memcpy(result->bytes, a->bytes, a->len);
	memcpy(result->bytes + a->len, b->bytes, b->len);
	string_rehash(result, ctx);
	return result;
}

static string_t* val_tostring(value_t, sylt_t*);

void sylt_concat(sylt_t* ctx) {
	value_t b = sylt_peek(ctx, 0);
	value_t a = sylt_peek(ctx, 1);
	value_t result = unit();
	
	if (a.tag == TYPE_LIST || b.tag == TYPE_LIST) {
		if (a.tag == TYPE_LIST && b.tag == TYPE_LIST) {
			result = wraplist(list_concat(getlist(a), getlist(b), ctx));
		} else if (a.tag == TYPE_LIST) {
			list_push(getlist(a), b, ctx);
			result = wraplist(getlist(a));
		} else if (b.tag == TYPE_LIST) {
			list_insert(getlist(b), 0, a, ctx);
			result = wraplist(getlist(b));
		}
		
	} else if (a.tag == TYPE_STRING || b.tag == TYPE_STRING) {
		gc_pause(ctx);
		result = wrapstring(string_concat(
			val_tostring(a, ctx), val_tostring(b, ctx), ctx));
		gc_resume(ctx);

	} else {
		halt(ctx, E_CONCAT_TYPE(a.tag, b.tag));
		unreachable();
	}
	
	sylt_shrink(ctx, 2); /* pop operands */
	sylt_push(ctx, result);
}

/* appends src to dst */
void string_append(string_t** dst, const string_t* src, sylt_t* ctx) {
	assert(dst && *dst && src);
	
	sylt_pushstring(ctx, *dst);
	sylt_pushstring(ctx, src);
	sylt_concat(ctx);
	*dst = sylt_popstring(ctx);
}

bool string_starts_with(const string_t* str, const string_t* other) {
	if (other->len > str->len)
		return false;
		
	return strncmp(
		(const char*)str->bytes,
		(const char*)other->bytes,
		other->len) == 0;
}

bool string_ends_with(const string_t* str, const string_t* other) {
	if (other->len > str->len)
		return false;
		
	return strncmp(
		(const char*)str->bytes + str->len - other->len,
		(const char*)other->bytes,
		other->len) == 0;
}

void string_print(string_t* str) {
	if (!str) {
		sylt_printf("NULL");
		return;
	}
	
	sylt_printf("%.*s", (int)str->len, str->bytes);
}

void string_eprint(string_t* str) {
	if (!str) {
		sylt_eprintf("NULL");
		return;
	}
	
	sylt_eprintf("%.*s", (int)str->len, str->bytes);
}

void string_dprint(string_t* str) {
	if (!str) {
		sylt_dprintf("NULL");
		return;
	}
	
	sylt_dprintf("%.*s", (int)str->len, str->bytes);
}

/* == function == */

/* creates a new function */
func_t* func_new(sylt_t* ctx, string_t* name, string_t* src) {
	func_t* func = (func_t*)obj_new(sizeof(func_t), TYPE_FUNCTION, ctx);
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
	func->src = src;
	func->cfunc = NULL;
	return func;
}

void func_free(func_t* func, sylt_t* ctx) {
	arr_free(func->code, uint8_t, nextpow2(func->ncode), ctx);
    arr_free(func->data, value_t, nextpow2(func->ndata), ctx);
    arr_free(func->lines, uint32_t, nextpow2(func->nlines), ctx);
	ptr_free(func, func_t, ctx);
}

/* writes a byte to the functions
 * bytecode array */
void func_write(func_t* func, uint8_t byte, uint32_t line, sylt_t* ctx) {
	if (func->ncode >= MAX_CODE) {
		halt(ctx, E_CODE_LIMIT);
		unreachable();
	}
	
	if (func->nlines >= MAX_LINES) {
		halt(ctx, E_LINE_LIMIT);
		unreachable();
	}
	
	/* write byte */
	func->code = arr_resize(
		func->code, uint8_t,
		nextpow2(func->ncode),
		nextpow2(func->ncode + 1),
		ctx);
	func->code[func->ncode++] = byte;
	
	/* write corresponding line number */
	func->lines = arr_resize(
		func->lines, uint32_t,
		nextpow2(func->nlines),
		nextpow2(func->nlines + 1),
		ctx);
	func->lines[func->nlines++] = line;
}

bool val_eq(value_t a, value_t b);

/* writes a value to a functions constant
 * data table and returns its index */
size_t func_write_data(func_t* func, value_t val, sylt_t* ctx) {
	for (size_t i = 0; i < func->ndata; i++)
		if (val_eq(func->data[i], val))
			return i;
	
	if (func->ndata >= MAX_DATA) {
		halt(ctx, E_DATA_LIMIT);
		unreachable();
	}
	
	sylt_push(ctx, val); /* GC */
	func->data = arr_resize(
		func->data,
		value_t,
		nextpow2(func->ndata),
		nextpow2(func->ndata + 1),
		ctx);
	sylt_pop(ctx); /* GC */
		
	func->data[func->ndata++] = val;
	return func->ndata - 1;
}

/* == closure == */

/* creates a new closure around a function */
closure_t* closure_new(sylt_t* ctx, const func_t* func) {
	closure_t* cls = (closure_t*)obj_new(sizeof(closure_t), TYPE_CLOSURE, ctx);
	cls->func = func;
	cls->upvals = NULL;
	cls->nupvals = func->upvalues;
	return cls;
}

void closure_free(closure_t* cls, sylt_t* ctx) {
	arr_free(cls->upvals, upvalue_t*, cls->nupvals, ctx);
	ptr_free(cls, closure_t, ctx);
}

/* == upvalue == */

/* creates a new upvalue */
upvalue_t* upvalue_new(sylt_t* ctx, size_t index) {
	upvalue_t* upval = (upvalue_t*)obj_new(sizeof(upvalue_t), TYPE_UPVALUE, ctx);
	upval->index = index;
	upval->closed = unit();
	upval->next = NULL;
	return upval;
}

void upvalue_free(upvalue_t* upval, sylt_t* ctx) {
	ptr_free(upval, upvalue_t, ctx);
}

/* == value == */

/* marks a value as reachable in case it
 * wraps an object */
void val_mark(value_t val, sylt_t* ctx) {
	if (!isheaptype(val.tag))
		return;
	obj_mark(getobj(val), ctx);
}

/* returns true if two values are equal */
bool val_eq(value_t a, value_t b) {
	if (a.tag != b.tag)
		return false;
	
	switch (a.tag) {
	case TYPE_UNIT: return true;
	case TYPE_BOOL: return getbool(a) == getbool(b);
	case TYPE_NUM: return getnum(a) == getnum(b);
	case TYPE_LIST: return list_eq(getlist(a), getlist(b));
	case TYPE_DICT: return dict_eq(getdict(a), getdict(b));
	case TYPE_STRING: return string_eq(getstring(a), getstring(b));
	case TYPE_FUNCTION:
	case TYPE_CLOSURE:
	case TYPE_UPVALUE: return getobj(a) == getobj(b);
	default: break;
	}

	unreachable();
	return false;
}

string_t* val_tostring_opts(value_t, bool, sylt_t*);

/* converts a value to a printable string */
static string_t* val_tostring(value_t val, sylt_t* ctx) {
	sylt_push(ctx, val); /* for GC */
	
	string_t* str = NULL;
	switch (val.tag) {
	case TYPE_UNIT: {
		str = string_lit("null", ctx);
		break;
	}
	case TYPE_BOOL: {
		if (getbool(val))
			str = string_lit("true", ctx);
		else
			str = string_lit("false", ctx);
		break;
	}
	case TYPE_NUM: {
		str = string_fmt(ctx, "%.8g", getnum(val));
		break;
	}
	case TYPE_LIST: {
		sylt_pushstring(ctx, string_lit("[", ctx));
		
		list_t* ls = getlist(val);
		for (size_t i = 0; i < ls->len; i++) {
			sylt_pushstring(ctx, val_tostring_opts(ls->items[i], true, ctx));
			sylt_concat(ctx);
			
			if (i < ls->len - 1) {
				sylt_pushstring(ctx, string_lit(" ", ctx));
				sylt_concat(ctx);
			}
		}
		
		sylt_pushstring(ctx, string_lit("]", ctx));
		sylt_concat(ctx);
		
		str = sylt_popstring(ctx);
		break;
	}
	case TYPE_DICT: {
		sylt_pushstring(ctx, string_lit("{", ctx));
		
		dict_t* dc = getdict(val);
		size_t n = 0;
		for (size_t i = 0; i < dc->cap; i++) {
			if (!dc->items[i].key)
				continue;
				
			if (val_eq(dc->items[i].val, wrapdict(dc)))
				continue;
			
			/* key */
			sylt_pushstring(ctx, dc->items[i].key);
			sylt_concat(ctx);
			
			sylt_pushstring(ctx, string_lit(" = ", ctx));
			sylt_concat(ctx);
			
			/* value */
			sylt_pushstring(ctx, val_tostring_opts(dc->items[i].val, true, ctx));
			sylt_concat(ctx);
			
			if (n < dc->len - 1) {
				sylt_pushstring(ctx, string_lit(" ", ctx));
				sylt_concat(ctx);
			}
			
			n++;
		}
		
		sylt_pushstring(ctx, string_lit("}", ctx));
		sylt_concat(ctx);
		
		str = sylt_popstring(ctx);
		break;
	}
	case TYPE_STRING: {
		str = getstring(val);
		break;
	}
	case TYPE_FUNCTION: {
		str = getfunc(val)->name;
		break;
	}
	case TYPE_CLOSURE: {
		str = getclosure(val)->func->name;
		break;
	}
	case TYPE_UPVALUE: {
		str = string_lit("<Upval>", ctx);
		break;
	}
	default: unreachable();
	}
	
	sylt_pop(ctx); /* val */
	return str;
}

string_t* val_tostring_opts(value_t val, bool quote_string, sylt_t* ctx) {
	string_t* str = val_tostring(val, ctx);
	if (quote_string && val.tag == TYPE_STRING)
		return string_fmt(ctx, "\"%.*s\"", (int)str->len, str->bytes);
	
	return str;
}

/* prints a value to stdout */
void val_print(value_t val, bool quote_string, sylt_t* ctx) {
	string_t* str = val_tostring_opts(val, quote_string, ctx);
	string_print(str);
}

void val_dprint(value_t val, bool quote_string, int max_length, sylt_t* ctx) {
	(void)max_length;
	string_t* str = val_tostring_opts(val, quote_string, ctx);
	sylt_dprintf("%.*s", (int)str->len, str->bytes);
}

/* == virtual machine == */

void vm_init(vm_t* vm, sylt_t* ctx) {
	vm->stack = NULL;
	vm->maxstack = 0;
	vm->sp = NULL;
	
	vm->nframes = 0;
	vm->fp = NULL;
	vm->gdict = dict_new(ctx);
	vm->openups = NULL;
	vm->hidden = unit();
	for (size_t i = 0; i < MAX_FILES; i++)
		vm->files[i] = NULL;
	vm->ctx = ctx;
	
	/* initialize some stack space */
	vm_ensure_stack(vm, SYLT_INIT_STACK);
}

void vm_free(vm_t* vm) {
	arr_free(vm->stack, value_t, vm->maxstack, vm->ctx);
	for (size_t i = 0; i < MAX_FILES; i++)
		if (vm->files[i])
			fclose(vm->files[i]);
}

size_t vm_address(const cframe_t* frame) {
	return (size_t)(frame->ip - 1 - frame->func->code);
}

uint32_t vm_line(const cframe_t* frame) {
	return frame->func->lines[vm_address(frame)];
}

/* VM macros */
#define read8() *vm->fp->ip++
#define read16() \
	(vm->fp->ip += 2, (uint16_t) \
		((vm->fp->ip[-2] << 8) \
			| vm->fp->ip[-1]))
#define readval() \
	vm->fp->func->data[read8()]
#define push(v) vm_push(vm, (v))
#define pop() vm_pop(vm)
#define peek(n) vm_peek(vm, (n))
#define shrink(n) vm_shrink(vm, (n))
#define func_stack() \
	(vm->stack + vm->fp->offs)

void dbg_print_header(const vm_t* vm, const closure_t* cls) {
	if (!vm->ctx->disassemble)
		return;
	
	const func_t* func = cls->func;
	
	sylt_dprintf("\n-> ");
	string_dprint(func->path);
	sylt_dprintf(" ");
	
	sylt_dprintf("(%d/%d), ",(int)vm->nframes, MAX_CFRAMES);
	
	size_t used = vm->sp - vm->stack;
	sylt_dprintf("stack %zu/%zu\n", used, vm->maxstack);
	
	sylt_dprintf(
		"%zu bytes, "
		"%zu constants, "
		"%zu slots, "
		"%d params, "
		"%zu upvals\n",
		func->ncode,
		func->ndata,
		func->slots,
		func->params,
		cls->nupvals);
	
	#if DBG_PRINT_DATA
	sylt_dprintf("  data:\n");
	for (size_t i = 0; i < func->ndata; i++) {
		string_t* str = val_tostring_opts(func->data[i], true, vm->ctx);
		sylt_dprintf("    #%-2ld -- ", i);
		string_dprint(str);
		sylt_dprintf("\n");
	}
	#endif
		
	sylt_dprintf(
		"  addr  line opcode             "
		"info\n");
	for (int i = 0; i < 40; i++)
		sylt_dprintf("-");
	sylt_dprintf("\n");
}

void dbg_print_instruction(const vm_t* vm) {
	if (!vm->ctx->disassemble)
		return;
	
	size_t addr = vm_address(vm->fp);
	sylt_dprintf("  %05zu", addr);
	
	/* line number */
	const uint32_t* lines =
		vm->fp->func->lines;
	uint32_t line = lines[addr];
	if (addr == 0 || line > lines[addr - 1])
		sylt_dprintf(" %-4d ", line);
	else
		sylt_dprintf(" |    ");
	
	/* name */
	uint8_t op = vm->fp->ip[-1];
	const char* name = OPINFO[op].name;
	sylt_dprintf("%-19s", name);
	
	/* extra information */
	uint8_t arg0 = *vm->fp->ip;
	switch (op) {
	case OP_PUSH: {
		value_t data = vm->fp->func->data[arg0];
		val_dprint(data, true, -1, vm->ctx);
		break;
	}
	case OP_PUSH_FUNC: {
		value_t func = vm->fp->func->data[arg0];
		val_dprint(func, true, -1, vm->ctx);
		break;
	}
	case OP_LOAD: {
		value_t val = func_stack()[arg0];
		val_dprint(val, true, -1, vm->ctx);
		break;
	}
	case OP_ADD_NAME: {
		value_t data = vm->fp->func->data[arg0];
		if (data.tag == TYPE_STRING) {
			string_t* name = getstring(data);
			value_t val = vm->sp[-1];
			
			sylt_dprintf("%.*s = ",
				(int)name->len, name->bytes);
			val_dprint(val, true, -1, vm->ctx);
		}
		break;
	}
	case OP_LOAD_NAME: {
		value_t data = vm->fp->func->data[arg0];
		if (data.tag == TYPE_STRING) {
			string_t* name = getstring(data);
			value_t* val = dict_get(vm->gdict, name);

			if (val) {
				sylt_dprintf("\"%.*s\" -> ",
					(int)name->len, name->bytes);
				val_dprint(*val, true, -1, vm->ctx);
			}
		}
		break;
	}
	case OP_STORE_NAME: {
		value_t data = vm->fp->func->data[arg0];
		if (data.tag == TYPE_STRING) {
			string_t* name = getstring(data);
			value_t val = vm->sp[-1];
			
			sylt_dprintf("%.*s <- ",
				(int)name->len, name->bytes);
			val_dprint(val, true, -1, vm->ctx);
		}
		break;
	}
	case OP_LOAD_KEY: {
		value_t collection = vm->sp[-2];
		dict_t* dc = getdict(collection);
		value_t* val = dict_get(dc, getstring(vm->sp[-1]));
		
		if (!val) {
			sylt_dprintf("<not found>");
		} else {
			val_dprint(*val, true, -1, vm->ctx);
		}
		break;
	}
	case OP_CALL: {
		sylt_dprintf("%d argument%s", arg0, (arg0 == 1) ? "" : "s");
		break;
	}
	}
		
	sylt_dprintf("\n");
}

void dbg_print_stack(const vm_t* vm, const func_t* func) {
	const int maxvals = 5;
	
	/* don't print first iteration */
	if (vm->fp->ip - 1 == func->code)
		return;
	
	gc_pause(vm->ctx);
	value_t* start = func_stack();
	
	/* only display the last N values */
	ptrdiff_t diff = vm->sp - start;
	if (diff > maxvals)
		start += diff - maxvals;
	
	value_t* v = start;
	
	if (diff > maxvals)
		sylt_dprintf("             [ <+%zu, ", diff - maxvals);
	else
		sylt_dprintf("             [ ");
	
	for (; v != vm->sp; v++) {
		val_print(*v, true, vm->ctx);
		if (v != vm->sp - 1)
		  sylt_dprintf(", ");
	}
	
	sylt_dprintf(" ]\n");
	gc_resume(vm->ctx);
}

/* pushes a value on the stack */
static inline void vm_push(vm_t* vm, value_t val) {
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
static inline void vm_shrink(vm_t* vm, int n) {
	assert(vm->sp - n >= vm->stack);
	vm->sp -= n;
}

/* returns the value n down the stack */
static inline value_t vm_peek(const vm_t* vm, int n) {
	value_t* ptr = vm->sp - 1 - n;
	assert(ptr >= vm->stack);
	assert(ptr <= vm->sp - 1);
	return *ptr;
}

/* returns the value referenced by
 * an upvalue */
value_t vm_load_upval(vm_t* vm, upvalue_t* upval) {
	if (upval->index == -1)
		return upval->closed;
	return vm->stack[upval->index];
}

/* sets the value referenced by an upvalue */
void vm_store_upval(vm_t* vm, upvalue_t* upval, value_t val) {
	if (upval->index == -1) {
		upval->closed = val;
		return;
	}
	
	vm->stack[upval->index] = val;
}

/* captures a stack variable into an
 * upvalue */
upvalue_t* vm_cap_upval(vm_t* vm, int index) {
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

/* closes any upvalues pointing at or above
 * the provided stack index */
void vm_close_upvals(vm_t* vm, int last) {
	while (vm->openups &&
		vm->openups->index >= last)
	{
		upvalue_t* upval = vm->openups;
		upval->closed = vm->stack[
			upval->index];
		upval->index = -1;
		vm->openups = upval->next;
	}
}

/* grows the stack if necessary */
void vm_ensure_stack(vm_t* vm, size_t needed) {
	needed += SYLT_EXTRA_STACK;
	if (vm->maxstack >= needed)
		return;
	
	size_t offset = vm->sp - vm->stack;
	
	gc_pause(vm->ctx);
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

void vm_exec(vm_t* vm) {
	set_state(vm->ctx, SYLT_STATE_EXEC);
	assert(vm->nframes > 0 && vm->fp);
	
	for (;;) {
		uint8_t op = read8();
		
		#if DBG_PRINT_STACK
		dbg_print_stack(vm,
			vm->fp->func);
		#endif
		
		dbg_print_instruction(vm);
		
		switch (op) {
		/* == stack == */
		case OP_PUSH: {
			push(readval());
			break;
		}
		case OP_PUSH_NULL: {
			push(unit());
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
				list_push(ls, peek(i), vm->ctx);
			pop(); /* GC */
			
			shrink(len);
			push(wraplist(ls));
			break;
		}
		case OP_PUSH_DICT: {
			uint8_t len = read8();
			dict_t* dc = dict_new(vm->ctx);
			
			push(wrapdict(dc)); /* GC */
			for (int i = len; i > 0; i -= 2)
				dict_set(dc, getstring(peek(i)), peek(i - 1), vm->ctx);
			pop(); /* GC */
			
			shrink(len);
			push(wrapdict(dc));
			break;
		}
		case OP_PUSH_FUNC: {
			gc_pause(vm->ctx);
			
			func_t* func = getfunc(readval());
			closure_t* cls = closure_new(vm->ctx, func);
			
			push(wrapclosure(cls));
			arr_alloc(cls->upvals, upvalue_t*, func->upvalues, vm->ctx);
			cls->nupvals = func->upvalues;
			
			/* read upvalues */
			for (size_t i = 0;
				i < cls->nupvals; i++)
			{
				uint8_t is_local = read8();
				uint8_t index = read8();
				
				if (is_local)
					cls->upvals[i] = vm_cap_upval(vm, vm->fp->offs + index);
				else
					cls->upvals[i] = vm->fp->cls->upvals[index];
			}
			
			gc_resume(vm->ctx);
			break;
		}
		case OP_POP: {
			pop();
			break;
		}
		case OP_POP_HEAP: {
			vm_close_upvals(vm, vm->sp - vm->stack - 1);
			pop();
			break;
		}
		case OP_DUP: {
			push(peek(0));
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
		case OP_ADD_NAME: {
			string_t* name = getstring(readval());
			dict_set(vm->gdict, name, pop(), vm->ctx);
			break;
		}
		case OP_ADD_MOD: {
			string_t* name = getstring(readval());
			string_t* mod_name = getstring(readval());

			dict_t* module = getdict(*dict_get(vm->gdict, mod_name));
			dict_set(module, name, pop(), vm->ctx);
			break;
        }
		case OP_LOAD_NAME: {
			string_t* name = getstring(readval());
			value_t* val = dict_get(vm->gdict, name);
				
			if (!val) {
				halt(vm->ctx, E_UNDEFINED(name));
				unreachable();
			}
			
			push(*val);
			break;
		}
		case OP_STORE_NAME: {
			string_t* name = getstring(readval());
			bool is_new = dict_set(vm->gdict, name, peek(0), vm->ctx);
				
			if (is_new) {
				halt(vm->ctx, E_UNDEFINED(name));
				unreachable();
			}
				
			break;
		}
		case OP_LOAD_KEY: {
			typecheck(vm->ctx, peek(1), TYPE_DICT);
			typecheck(vm->ctx, peek(0), TYPE_STRING);
			
			string_t* key = getstring(pop());
			dict_t* dc = getdict(pop());
			value_t* val = dict_get(dc, key);
			
			if (!val) {
				push(unit());
				halt(vm->ctx, E_KEY_NOT_FOUND(key));
				unreachable();
			}	
			
			push(*val);
			break;
		}
		case OP_STORE_KEY: {
			value_t val = pop();
			typecheck(vm->ctx, peek(0), TYPE_STRING);
			typecheck(vm->ctx, peek(1), TYPE_DICT);

			string_t* key = getstring(pop());
			dict_t* dc = getdict(pop());
			dict_set(dc, key, val, vm->ctx);
			push(val);
			break;
		}
		case OP_LOAD_UPVAL: {
			value_t val = vm_load_upval(vm, vm->fp->cls->upvals[read8()]);
			push(val);
			break;
		}
		case OP_STORE_UPVAL: {
			vm_store_upval(vm, vm->fp->cls->upvals[read8()], peek(0));
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
		case OP_ADD: {
			if (peek(0).tag == TYPE_NUM && peek(1).tag == TYPE_NUM) {
				sylt_num_t b = getnum(pop());
				sylt_num_t a = getnum(pop());
				push(wrapnum(a + b));
				break;
			}
				
			sylt_concat(vm->ctx);
			break;
		}
		case OP_SUB: {
			if (peek(0).tag == TYPE_LIST) {
				/* n - list (pop start) */
				list_t* ls = getlist(pop());
				sylt_num_t n = getnum(pop());
				for (int64_t i = 0; i < n; i++)
					list_delete(ls, 0, vm->ctx);
				push(wraplist(ls));
				break;

			} else if (peek(1).tag == TYPE_LIST) {
				/* list - n (pop end) */
				sylt_num_t n = getnum(pop());
				list_t* ls = getlist(pop());
				for (int64_t i = 0; i < n; i++)
					list_pop(ls, vm->ctx);
				push(wraplist(ls));
				break;
			}

			typecheck(vm->ctx, peek(0), TYPE_NUM);
			typecheck(vm->ctx, peek(1), TYPE_NUM);
			sylt_num_t b = getnum(pop());
			sylt_num_t a = getnum(pop());
			push(wrapnum(a - b));
			break;
		}
		case OP_MUL: {
			typecheck(vm->ctx, peek(0), TYPE_NUM);
			typecheck(vm->ctx, peek(1), TYPE_NUM);
			sylt_num_t b = getnum(pop());
			sylt_num_t a = getnum(pop());
			push(wrapnum(a * b));
			break;
		}
		case OP_DIV: {
			typecheck(vm->ctx, peek(0), TYPE_NUM);
			typecheck(vm->ctx, peek(1), TYPE_NUM);
			sylt_num_t b = getnum(pop());
			sylt_num_t a = getnum(pop());
			
			if (b == 0.0f) {
				halt(vm->ctx, E_DIV_BY_ZERO);
				unreachable();
			}
			
			push(wrapnum(a / b));
			break;
		}
		case OP_EDIV: {
			typecheck(vm->ctx, peek(0), TYPE_NUM);
			typecheck(vm->ctx, peek(1), TYPE_NUM);
			sylt_num_t b = getnum(pop());
			sylt_num_t a = getnum(pop());
			
			if (b == 0.0f) {
				halt(vm->ctx, E_DIV_BY_ZERO);
				unreachable();
			}
		
			sylt_num_t res = num_func(fmodf, fmod)(a, b);
			if (res < 0)
				res += fabs(b);
			push(wrapnum(res));
			break;
		}
		case OP_UMIN: {
			typecheck(vm->ctx, peek(0), TYPE_NUM);
			push(wrapnum(-getnum(pop())));
			break;
		}
		case OP_LENGTH: {
			value_t length_of = pop();
			if (length_of.tag == TYPE_LIST)
				push(wrapnum(getlist(length_of)->len));
			else if (length_of.tag == TYPE_DICT)
				push(wrapnum(getdict(length_of)->len));
			else if (length_of.tag == TYPE_STRING)
				push(wrapnum(getstring(length_of)->len));
			else
				halt(vm->ctx, E_NO_LENGTH(length_of.tag));
			break;
		}
		/* == comparison == */
		case OP_LT: {
			typecheck(vm->ctx, peek(0), TYPE_NUM);
			typecheck(vm->ctx, peek(1), TYPE_NUM);
			sylt_num_t b = getnum(pop());
			sylt_num_t a = getnum(pop());
			push(wrapbool(a < b));
			break;
		}
		case OP_LTE: {
			typecheck(vm->ctx, peek(0), TYPE_NUM);
			typecheck(vm->ctx, peek(1), TYPE_NUM);
			sylt_num_t b = getnum(pop());
			sylt_num_t a = getnum(pop());
			push(wrapbool(a <= b));
			break;
		}
		case OP_GT: {
			typecheck(vm->ctx, peek(0), TYPE_NUM);
			typecheck(vm->ctx, peek(1), TYPE_NUM);
			sylt_num_t b = getnum(pop());
			sylt_num_t a = getnum(pop());
			push(wrapbool(a > b));
			break;
		}
		case OP_GTE: {
			typecheck(vm->ctx, peek(0), TYPE_NUM);
			typecheck(vm->ctx, peek(1), TYPE_NUM);
			sylt_num_t b = getnum(pop());
			sylt_num_t a = getnum(pop());
			push(wrapbool(a >= b));
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
			typecheck(vm->ctx, peek(0), TYPE_BOOL);
			push(wrapbool(!getbool(pop())));
			break;
		}
		/* == control flow == */
		case OP_JMP: {
			uint16_t offset = read16();
			vm->fp->ip += offset;
			break;
		}
		case OP_JMP_IF: {
			uint16_t offset = read16();
			typecheck(vm->ctx, peek(0), TYPE_BOOL);
			if (getbool(peek(0)))
				vm->fp->ip += offset;
			break;
		}
		case OP_JMP_IF_NOT: {
			uint16_t offset = read16();
			typecheck(vm->ctx, peek(0), TYPE_BOOL);
			if (!getbool(peek(0)))
				vm->fp->ip += offset;
			break;
		}
		case OP_JMP_BACK: {
			uint16_t offset = read16();
			vm->fp->ip -= offset;
			break;
		}
		case OP_CALL: {
			sylt_call(vm->ctx, read8());
			break;
		}
		case OP_RET: {
			value_t ret = pop();
			vm_close_upvals(vm, vm->fp->offs);
			vm->sp = func_stack() - 1;
			push(ret);
			
			vm->nframes--;
			if (vm->nframes == 0) {
				set_state(vm->ctx, SYLT_STATE_DONE);
				return;
			}

			vm->fp = &vm->frames[vm->nframes - 1];
			
			dbg_print_header(vm, vm->fp->cls);
			break;
		}
		default: unreachable();
		}
	}
}

void sylt_call(sylt_t* ctx, int argc) {
	vm_t* vm = ctx->vm;
	
	if (peek(argc).tag != TYPE_CLOSURE)
		halt(ctx, E_NOT_CALLABLE(peek(argc).tag));

	closure_t* cls = getclosure(peek(argc));
	const func_t* func = cls->func;
				
	if (func->params != argc) {
		halt(vm->ctx, E_WRONG_ARGC(func->name, func->params, argc));
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
				
		if (ctx->disassemble)
			sylt_dprintf("\n");
				
		gc_resume(vm->ctx);
		return; /* early return */
	}
			
	if (vm->nframes == MAX_CFRAMES) {
		halt(vm->ctx, E_STACK_OVERFLOW, MAX_CFRAMES);
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
				
	dbg_print_header(vm, vm->fp->cls);
}

#undef read8
#undef read16
#undef readval
#undef push
#undef pop
#undef peek
#undef shrink
#undef func_stack

/* == standard library == */

#define argc() (ctx->vm->fp->ip[-1])
#define arg(n) sylt_peek(ctx, argc() - (n) - 1)

#define boolarg(n) getbool(arg(n))
#define numarg(n) getnum(arg(n))
#define listarg(n) getlist(arg(n))
#define dictarg(n) getdict(arg(n))
#define stringarg(n) getstring(arg(n))
#define closurearg(n) getclosure(arg(n))

#define argcheck(ctx, n, t, fname) \
	if (arg(n).tag != t) \
		halt(ctx, E_ARG_TYPE(t, arg(n).tag, n, fname))

/* == prelude lib == */

value_t std_print(sylt_t* ctx) {
	val_print(arg(0), false, ctx);
	fflush(stdout);
	return unit();
}

value_t std_print_ln(sylt_t* ctx) {
	val_print(arg(0), false, ctx);
	sylt_printf("\n");
	return unit();
}

value_t std_read_in(sylt_t* ctx) {
	char buffer[8192];
	fgets(buffer, 8192, stdin);
	
	/* get rid of trailing newline */
	buffer[strlen(buffer) - 1] = '\0';
	return wrapstring(string_lit(buffer, ctx));
}

value_t std_to_string(sylt_t* ctx) {
	string_t* str = val_tostring(arg(0), ctx);
	return wrapstring(str);
}

value_t std_to_num(sylt_t* ctx) {
	sylt_num_t num;
	
	switch (arg(0).tag) {
	case TYPE_BOOL: num = boolarg(0); break;
	case TYPE_NUM: num = numarg(0); break;
	case TYPE_STRING:
		num = num_func(strtof, strtod)((char*)stringarg(0)->bytes, NULL);
		break;
	default: num = 0;
	}
	
	return wrapnum(num);
}

value_t std_float_eq(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	argcheck(ctx, 2, TYPE_NUM, __func__);
	
	sylt_num_t a = numarg(0);
	sylt_num_t b = numarg(1);
	if (a == b)
		return wrapbool(true);
	
	sylt_num_t epsilon = getnum(arg(2));
	sylt_num_t diff = num_func(fabsf, fabs)(a - b);
	
	return wrapbool(diff < epsilon);
}

value_t std_type_of(sylt_t* ctx) {
	return wrapstring(string_lit(user_type_name(arg(0).tag), ctx));
}

value_t std_ensure(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_BOOL, __func__);
	if (!boolarg(0)) {
		halt(ctx, E_ENSURE_FAILED);
		unreachable();
	}
	
	return arg(0);
}

value_t std_todo(sylt_t* ctx) {
	halt(ctx, E_TODO_REACHED);
	return unit();
}

value_t std_unreachable(sylt_t* ctx) {
	halt(ctx, E_UNREACHABLE_REACHED);
	return unit();
}

value_t std_bit_shift_left(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	return wrapnum((int64_t)numarg(0) << (int64_t)numarg(1));
}

value_t std_bit_shift_right(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	return wrapnum((int64_t)numarg(0) >> (int64_t)numarg(1));
}

value_t std_bit_and(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	return wrapnum((int64_t)numarg(0) & (int64_t)numarg(1));
}

value_t std_bit_or(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	return wrapnum((int64_t)numarg(0) | (int64_t)numarg(1));
}

value_t std_bit_xor(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	return wrapnum((int64_t)numarg(0) ^ (int64_t)numarg(1));
}

value_t std_bit_not(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(~(int64_t)numarg(0));
}

/* == system lib == */

value_t stdsys_mem_info(sylt_t* ctx) {
	dict_t* dc = dict_new(ctx);
	dict_set(dc, string_lit("memUse", ctx), wrapnum(ctx->mem.bytes), ctx);
	dict_set(dc, string_lit("topMemUse", ctx), wrapnum(ctx->mem.highest), ctx);
	dict_set(dc, string_lit("gcCycles", ctx), wrapnum(ctx->mem.gc.cycles), ctx);
	dict_set(dc, string_lit("nextGC", ctx), wrapnum(ctx->mem.gc.trigger), ctx);
	return wrapdict(dc);
}

value_t stdsys_mem_sizes(sylt_t* ctx) {
	dict_t* dc = dict_new(ctx);
	dict_set(dc, string_lit("char", ctx), wrapnum(sizeof(char)), ctx);
	dict_set(dc, string_lit("bool", ctx), wrapnum(sizeof(bool)), ctx);
	dict_set(dc, string_lit("int", ctx), wrapnum(sizeof(int)), ctx);
	dict_set(dc, string_lit("long", ctx), wrapnum(sizeof(long)), ctx);
	dict_set(dc, string_lit("size_t", ctx), wrapnum(sizeof(size_t)), ctx);
	dict_set(dc, string_lit("value_t", ctx), wrapnum(sizeof(value_t)), ctx);
	dict_set(dc, string_lit("list_t", ctx), wrapnum(sizeof(list_t)), ctx);
	dict_set(dc, string_lit("dict_t", ctx), wrapnum(sizeof(dict_t)), ctx);
	dict_set(dc, string_lit("string_t", ctx), wrapnum(sizeof(string_t)), ctx);
	dict_set(dc, string_lit("func_t", ctx), wrapnum(sizeof(func_t)), ctx);
	dict_set(dc, string_lit("closure_t", ctx), wrapnum(sizeof(closure_t)), ctx);
	dict_set(dc, string_lit("upvalue_t", ctx), wrapnum(sizeof(upvalue_t)), ctx);
	return wrapdict(dc);
}

value_t stdsys_src(sylt_t* ctx) {
	dict_t* dc = dict_new(ctx);
	dict_set(dc,string_lit("text", ctx), wrapstring(ctx->vm->fp->func->src), ctx);
	dict_set(dc, string_lit("name", ctx), wrapstring(ctx->vm->fp->func->name), ctx);
	dict_set(dc, string_lit("path", ctx), wrapstring(ctx->vm->fp->func->path), ctx);
	dict_set(dc, string_lit("line", ctx), wrapnum(vm_line(ctx->vm->fp)), ctx);
	return wrapdict(dc);
}

value_t stdsys_exec(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	system((char*)stringarg(0)->bytes);
	return unit();
}

value_t stdsys_halt(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	halt(ctx, (const char*)stringarg(0)->bytes);
	return unit();
}

value_t stdsys_time_format(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* fmt = stringarg(0);
	
	time_t timer;
    char buffer[64];
    struct tm* tm_info;
    
    timer = time(NULL);
    tm_info = localtime(&timer);
    strftime(buffer, 64, (const char*)fmt->bytes, tm_info);
    
    string_t* str = string_lit(buffer, ctx);
    return wrapstring(str);
}

value_t stdsys_time_stamp(sylt_t* ctx) {
	(void)ctx;
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return wrapnum(ts.tv_nsec);
}

value_t stdsys_cpu_clock(sylt_t* ctx) {
	(void)ctx;
	return wrapnum(
		(double)clock() / CLOCKS_PER_SEC);
}

/* == list lib == */

value_t stdlist_init(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	list_t* ls = list_new(ctx);
	for (size_t i = 0; i < numarg(0); i++)
		list_push(ls, arg(1), ctx);
	return wraplist(ls);
}

value_t stdlist_get(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_LIST, __func__);
	return list_get(listarg(1), numarg(0), ctx);
}

value_t stdlist_set(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 2, TYPE_LIST, __func__);
	list_set(listarg(2), numarg(0), arg(1), ctx);
	return unit();
}

value_t stdlist_swap(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	argcheck(ctx, 2, TYPE_LIST, __func__);
	sylt_num_t a = numarg(0);
	sylt_num_t b = numarg(1);
	list_t* ls = listarg(2);

	value_t tmp = list_get(ls, b, ctx);
	list_set(ls, b, list_get(ls, a, ctx), ctx);
	list_set(ls, a, tmp, ctx);
	return unit();
}

value_t stdlist_add(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 2, TYPE_LIST, __func__);
	list_insert(listarg(2), numarg(0), arg(1), ctx);
	return unit();
}

value_t stdlist_push(sylt_t* ctx) {
	argcheck(ctx, 1, TYPE_LIST, __func__);
	list_push(listarg(1), arg(0), ctx);
	return unit();
}

value_t stdlist_del(sylt_t* ctx) {
	argcheck(ctx, 1, TYPE_LIST, __func__);
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return list_delete(listarg(1), numarg(0), ctx);
}

value_t stdlist_pop(sylt_t* ctx) {
	return list_pop(listarg(0), ctx);
}

value_t stdlist_count(sylt_t* ctx) {
	argcheck(ctx, 1, TYPE_LIST, __func__);
	size_t result = list_count(listarg(1), arg(0));
	return wrapnum(result);
}

value_t stdlist_contains(sylt_t* ctx) {
	argcheck(ctx, 1, TYPE_LIST, __func__);
	bool result = list_count(listarg(1), arg(0)) > 0;
	return wrapbool(result);
}

value_t stdlist_find(sylt_t* ctx) {
    argcheck(ctx, 1, TYPE_LIST, __func__);
	list_t* ls = listarg(1);
	for (size_t i = 0; i < ls->len; i++)
        if (val_eq(arg(0), ls->items[i]))
			return wrapnum(i);
	return wrapnum(-1);
}

value_t stdlist_rev(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_LIST, __func__);
	
	list_t* old_ls = listarg(0);
	if (old_ls->len == 0)
		return arg(0);
	
	list_t* new_ls = list_new(ctx);
	for (size_t i = 0; i < old_ls->len; i++)
		list_push(new_ls, old_ls->items[old_ls->len - i - 1], ctx);
	
	return wraplist(new_ls);
}

value_t stdlist_range(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	sylt_num_t start = numarg(0);
	sylt_num_t end = numarg(1);

	list_t* ls = list_new(ctx);
	for (int64_t i = start; i < end; i++)
		list_push(ls, wrapnum(i), ctx);
	
	return wraplist(ls);
}

/* == dict lib == */

value_t stddict_get(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 1, TYPE_DICT, __func__);
	value_t* val = dict_get(dictarg(1), stringarg(0));

	if (!val) {
		halt(ctx, E_KEY_NOT_FOUND(stringarg(0)));
		unreachable();
	}

	return *val;
}

value_t stddict_set(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 2, TYPE_DICT, __func__);
	dict_set(dictarg(2), stringarg(0), arg(1), ctx);
	return unit();
}

value_t stddict_has_key(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 1, TYPE_DICT, __func__);
	return wrapbool(dict_get(dictarg(1), stringarg(0)) != NULL);
}

value_t stddict_keys(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_DICT, __func__);
	dict_t* dc = dictarg(0);
	list_t* keys = list_new(ctx);
	for (size_t i = 0; i < dc->cap; i++) {
		if (!dc->items[i].key)
			continue;
		list_push(keys, wrapstring(dc->items[i].key), ctx);
	}
	
	return wraplist(keys);
}

value_t stddict_values(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_DICT, __func__);
	dict_t* dc = dictarg(0);
	list_t* values = list_new(ctx);
	for (size_t i = 0; i < dc->cap; i++) {
		if (!dc->items[i].key)
			continue;
		list_push(values, dc->items[i].val, ctx);
	}
	
	return wraplist(values);
}

/* == string lib == */

value_t stdstring_chars(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* str = stringarg(0);
	list_t* ls = list_new(ctx);
	
	for (size_t i = 0; i < str->len; i++) {
		string_t* ch = string_new(&str->bytes[i], 1, ctx);
		list_push(ls, wrapstring(ch), ctx);
	}
	
	return wraplist(ls);
}

value_t stdstring_ascii(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* str = stringarg(0);
	list_t* ls = list_new(ctx);
	
	for (size_t i = 0; i < str->len; i++) {
		list_push(ls, wrapnum(str->bytes[i]), ctx);
	}
	
	return wraplist(ls);
}

value_t stdstring_join(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_LIST, __func__);
	
	list_t* ls = listarg(0);
	string_t* str = string_new(NULL, 0, ctx);
	sylt_pushstring(ctx, str);
	
	for (size_t i = 0; i < ls->len; i++) {
		string_t* val_str = val_tostring(ls->items[i], ctx);
		
		sylt_pushstring(ctx, str);
		sylt_pushstring(ctx, val_str);
		sylt_concat(ctx);
		
		str = sylt_popstring(ctx);
	}
	
	sylt_pop(ctx);
	return wrapstring(str);
}

value_t stdstring_split(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_LIST, __func__);
	argcheck(ctx, 1, TYPE_STRING, __func__);
	
	list_t* separators = listarg(0);
	string_t* str = stringarg(1);
	list_t* parts = list_new(ctx);
	
	if (str->len == 0)
		return wraplist(parts);
	
	string_t* current = string_new(NULL, 0, ctx);
	
	for (size_t i = 0; i < str->len; i++) {
		string_t* c = string_new((unsigned char*)str->bytes + i, 1, ctx);

		bool split = false;
		for (size_t j = 0; j < separators->len; j++) {
			string_t* sep = val_tostring(separators->items[j], ctx);
			split = sep->len > 0 && str->bytes[i] == sep->bytes[0];
			if (split)
				break;
		}
			
		if (!split) {
			sylt_pushstring(ctx, current);
			sylt_pushstring(ctx, c);
			sylt_concat(ctx);
			current = sylt_popstring(ctx);
		}
		
		if ((split || i == str->len - 1) && current->len > 0) {
			list_push(parts, wrapstring(current), ctx);
			current = string_new(NULL, 0, ctx);
		}
	}
	
	return wraplist(parts);
}

value_t stdstring_lowercase(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* copy = string_new(stringarg(0)->bytes, stringarg(0)->len, ctx);
	
	for (size_t i = 0; i < copy->len; i++) {
		copy->bytes[i] = tolower(copy->bytes[i]);
	}
	
	string_rehash(copy, ctx);
	return wrapstring(copy);
}

value_t stdstring_uppercase(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* copy = string_new(stringarg(0)->bytes, stringarg(0)->len, ctx);
	
	for (size_t i = 0; i < copy->len; i++)
		copy->bytes[i] = toupper(copy->bytes[i]);
	
	string_rehash(copy, ctx);
	return wrapstring(copy);
}

value_t stdstring_swapcase(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	
	string_t* copy = string_new(stringarg(0)->bytes, stringarg(0)->len, ctx);
	
	for (size_t i = 0; i < copy->len; i++) {
		char c = copy->bytes[i];
		copy->bytes[i] = (isupper(c)) ? tolower(c) : toupper(c);
	}
	
	string_rehash(copy, ctx);
	return wrapstring(copy);
}

value_t stdstring_is_lower(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* str = stringarg(0);

	bool is_lower = true;
	for (size_t i = 0; i < str->len; i++) {
		if (!islower(str->bytes[i])) {
			is_lower = false;
			break;
		}
	}

	return wrapbool(is_lower);
}

value_t stdstring_is_upper(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* str = stringarg(0);

	bool is_upper = true;
	for (size_t i = 0; i < str->len; i++) {
		if (!isupper(str->bytes[i])) {
			is_upper = false;
			break;
		}
	}

	return wrapbool(is_upper);
}

value_t stdstring_is_whitespace(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* str = stringarg(0);

	bool is_whitespace = true;
	for (size_t i = 0; i < str->len; i++) {
		if (!isspace(str->bytes[i])) {
			is_whitespace = false;
			break;
		}
	}

	return wrapbool(is_whitespace);
}

value_t stdstring_starts_with(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 1, TYPE_STRING, __func__);
	
	string_t* other = stringarg(0);
	string_t* str = stringarg(1);
	return wrapbool(string_starts_with(str, other));
}

value_t stdstring_ends_with(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 1, TYPE_STRING, __func__);
	
	string_t* other = stringarg(0);
	string_t* str = stringarg(1);
	return wrapbool(string_ends_with(str, other));
}

value_t stdstring_trim_start(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* src = stringarg(0);
	string_t* dst = string_new(src->bytes, src->len, ctx);
	
	size_t len = 0;
	for (size_t i = 0; i < dst->len; i++) {
		if (isspace(src->bytes[i]))
			continue;
		dst->bytes[len++] = src->bytes[i];
	}
	
	dst->bytes = arr_resize(dst->bytes, uint8_t, dst->len, len, ctx);
	dst->len = len;
	
	string_rehash(dst, ctx);
	return wrapstring(dst);
}

value_t stdstring_trim_end(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* src = stringarg(0);
	string_t* dst = string_new(src->bytes, src->len, ctx);
	
	size_t len = 0;
	for (int i = dst->len - 1; i >= 0; i--) {
		if (isspace(src->bytes[i]))
			continue;
		dst->bytes[dst->len - (++len)] = src->bytes[i];
	}
	
	dst->bytes = arr_resize(dst->bytes, uint8_t, dst->len, len, ctx);
	dst->len = len;
	
	string_rehash(dst, ctx);
	return wrapstring(dst);
}

value_t stdstring_trim(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	value_t res = stdstring_trim_end(ctx);
	res = stdstring_trim_start(ctx);
	return res;
}

value_t stdstring_find(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 1, TYPE_STRING, __func__);
	argcheck(ctx, 2, TYPE_NUM, __func__);

	string_t* str = stringarg(0);
	string_t* find = stringarg(1);
	size_t start = (size_t)numarg(2);

	for (size_t i = start; i < str->len; i++) {
		bool found_match =
			find->len <= str->len - i
			&& strncmp(
				(const char*)str->bytes + i,
				(const char*)find->bytes,
				find->len) == 0;
		
		if (found_match)
			return wrapnum(i);
	}

	return unit();
}

value_t stdstring_replace(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 1, TYPE_STRING, __func__);
	argcheck(ctx, 2, TYPE_STRING, __func__);
	
	string_t* find = stringarg(0);
	string_t* replace = stringarg(1);
	string_t* str = stringarg(2);
	string_t* new_str = string_lit("", ctx);
		
	for (size_t i = 0; i < str->len;) {
		bool found_match =
			find->len <= str->len - i
			&& strncmp(
				(const char*)str->bytes + i,
				(const char*)find->bytes,
				find->len) == 0;
		
		if (found_match) {
			sylt_pushstring(ctx, new_str);
			sylt_pushstring(ctx, replace);
			sylt_concat(ctx);
			new_str = sylt_popstring(ctx);
			i += find->len;
			continue;
		}
		
		sylt_pushstring(ctx, new_str);
		sylt_pushstring(ctx, string_new((unsigned char*)str->bytes + i, 1, ctx));
		sylt_concat(ctx);
		new_str = sylt_popstring(ctx);
		i++;
	}
	
	string_rehash(new_str, ctx);
	return wrapstring(new_str);
}

/* == file lib == */

value_t stdfile_open(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	
	string_t* path = stringarg(0);
	int64_t mode = (int64_t)numarg(1);
	const char* mode_str = NULL;

	switch (mode) {
	case 0: mode_str = "r"; break;
	case 1: mode_str = "rb"; break; 
	case 2: mode_str = "w"; break; 
	case 3: mode_str = "wb"; break; 
	case 4: mode_str = "a"; break; 
	case 5: mode_str = "ab"; break; 
	default: unreachable();
	}
	
	/* get a handle from the VM */
	int handle = -1;
	for (size_t i = 1; i < MAX_FILES; i++)
		if (!ctx->vm->files[i]) {
			handle = i;
			break;
		}
	
	if (handle == -1) halt(ctx, E_OPEN_FAILED(path));
	
	ctx->vm->files[handle] = fopen((const char*)path->bytes, mode_str);
	return wrapnum(handle);
}

value_t stdfile_close(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	int64_t handle = (int64_t)numarg(0);

	FILE* fp = ctx->vm->files[handle];
	if (!fp) halt(ctx, E_INVALID_HANDLE(handle));
	ctx->vm->files[handle] = NULL;
		
	fclose(fp);
	return unit();
}

value_t stdfile_read(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	
	int64_t handle = (int64_t)numarg(0);
	FILE* fp = ctx->vm->files[handle];
	if (!fp) halt(ctx, E_INVALID_HANDLE(handle));
	
	fseek(fp, 0, SEEK_END);
	size_t size = ftell(fp);
	fseek(fp, 0, SEEK_SET);

	string_t* str = string_new(NULL, size, ctx);
	fread(str->bytes, 1, size, fp);
	string_rehash(str, ctx);

	return wrapstring(str);
}

value_t stdfile_write(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	
	int64_t handle = (int64_t)numarg(1);
	FILE* fp = ctx->vm->files[handle];
	if (!fp)
		halt(ctx, E_INVALID_HANDLE(handle));
	
	string_t* str = stringarg(0);
	for (size_t i = 0; i < str->len; i++)
		putc(str->bytes[i], fp);
	
	return unit();
}

value_t stdfile_size(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	
	int64_t handle = (int64_t)numarg(0);
	FILE* fp = ctx->vm->files[handle];
	if (!fp)
		halt(ctx, E_INVALID_HANDLE(handle));
	
	fseek(fp, 0, SEEK_END);
	size_t size = ftell(fp);
	fseek(fp, 0, SEEK_SET);
	return wrapnum(size);
}

value_t stdfile_list_dir(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);

	list_t* ls = list_new(ctx);
	struct dirent* dir;
	DIR* d = opendir((const char*)stringarg(0)->bytes);
	if (d) {
		while ((dir = readdir(d)) != NULL) {
			if (strncmp(dir->d_name, ".", 1) == 0) continue;
			if (strncmp(dir->d_name, "..", 2) == 0) continue;

			string_t* str = string_lit(dir->d_name, ctx);
			list_push(ls, wrapstring(str), ctx);
		}

		closedir(d);
	}

	return wraplist(ls);
}

value_t stdfile_del(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_STRING, __func__);
	string_t* path = stringarg(0);
	remove((char*)path->bytes);
	return unit();
}

/* == math lib == */

#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795
#endif

#ifndef M_E
#define M_E 2.7182818284590452353602874713526
#endif

value_t stdmath_abs(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	sylt_num_t result =
		num_func(fabsf, fabs)(numarg(0));
	return wrapnum(result);
}

value_t stdmath_log(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	
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
		result = num_func(logf, log)(x) / num_func(logf, log)(base);
	}
	
	return wrapnum(result);
}

value_t stdmath_pow(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	sylt_num_t base = numarg(0);
	sylt_num_t exp = numarg(1);
	sylt_num_t result = num_func(powf, pow)(base, exp);
	return wrapnum(result);
}

value_t stdmath_sqrt(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	sylt_num_t result = num_func(sqrtf, sqrt)(numarg(0));
	return wrapnum(result);
}

value_t stdmath_floor(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(floorf, floor)(numarg(0)));
}

value_t stdmath_ceil(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(ceilf, ceil)(numarg(0)));
}

value_t stdmath_round(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(roundf, round)(numarg(0)));
}

value_t stdmath_rad(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(numarg(0) * M_PI / 180.0);
}

value_t stdmath_deg(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(numarg(0) * 180.0 / M_PI);
}

value_t stdmath_sin(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(sinf, sin)(numarg(0)));
}

value_t stdmath_cos(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(cosf, cos)(numarg(0)));
}

value_t stdmath_tan(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(tanf, tan)(numarg(0)));
}

value_t stdmath_asin(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(asinf, asin)(numarg(0)));
}

value_t stdmath_acos(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(acosf, acos)(numarg(0)));
}

value_t stdmath_atan(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(atanf, atan)(numarg(0)));
}

value_t stdmath_sinh(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(sinhf, sinh)(numarg(0)));
}

value_t stdmath_cosh(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(coshf, cosh)(numarg(0)));
}

value_t stdmath_tanh(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(tanhf, tanh)(numarg(0)));
}

value_t stdmath_asinh(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(asinhf, asinh)(numarg(0)));
}

value_t stdmath_acosh(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(acoshf, acosh)(numarg(0)));
}

value_t stdmath_atanh(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	return wrapnum(num_func(atanhf, atanh)(numarg(0)));
}

/* == rand library == */

value_t stdrand_range(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	argcheck(ctx, 1, TYPE_NUM, __func__);
	sylt_num_t start = numarg(0);
	sylt_num_t end = numarg(1);
	
    return wrapnum(start + rand() / (RAND_MAX / (end - start)));
}

value_t stdrand_seed(sylt_t* ctx) {
	argcheck(ctx, 0, TYPE_NUM, __func__);
	srand(numarg(0));
	return unit();
}

static string_t* lib_name = NULL;
static dict_t* lib_dict = NULL;

void std_setlib(
	sylt_t* ctx, const char* lib)
{
    lib_dict = dict_new(ctx);
	if (strlen(lib) == 0)
		lib_name = NULL;
	else
		lib_name = string_lit(lib, ctx);
}

void std_add(sylt_t*, const char*, value_t);

void std_addlib(sylt_t* ctx) {
	if (!lib_name) {
		return;
		lib_name = string_lit("Prelude", ctx);
	}
	
	dict_set(ctx->vm->gdict, lib_name, wrapdict(lib_dict), ctx);
}

/* adds a value to the standard library */
void std_add(sylt_t* ctx, const char* name, value_t val) {
	string_t* name_str = string_lit(name, ctx);
	dict_set(lib_dict, name_str, val, ctx);
	
	/* for prelude library */
	if (!lib_name)
		dict_set(ctx->vm->gdict, name_str, val, ctx);
}

/* adds a function to the standard library */
void std_addf(sylt_t* ctx, const char* name, cfunc_t cfunc, int params) {
	/* hide from GC */
	sylt_pushstring(ctx, string_lit(name, ctx));
	
	sylt_pushfunc(ctx, func_new(ctx, sylt_peekstring(ctx, 0), NULL));
	sylt_peekfunc(ctx, 0)->cfunc = cfunc;
	sylt_peekfunc(ctx, 0)->params = params;
	
	sylt_pushclosure(ctx, closure_new(ctx, sylt_peekfunc(ctx, 0)));
	
	std_add(ctx, name, sylt_peek(ctx, 0));
	
	/* safe */
	sylt_shrink(ctx, 3);
}

/* loads the standard library into
 * global memory */
void std_init(sylt_t* ctx) {
	gc_pause(ctx);
	
	/* prelude */
	std_setlib(ctx, "");
	std_add(ctx, "gdict", wrapdict(ctx->vm->gdict));
	std_addf(ctx, "print", std_print, 1);
	std_addf(ctx, "printLn", std_print_ln, 1);
	std_addf(ctx, "readIn", std_read_in, 0);
	std_addf(ctx, "toString", std_to_string, 1);
	std_addf(ctx, "toNum", std_to_num, 1);
	std_addf(ctx, "floatEq", std_float_eq, 3);
	std_addf(ctx, "typeOf", std_type_of, 1);
	std_addf(ctx, "ensure", std_ensure, 1);
	std_addf(ctx, "todo", std_todo, 0);
	std_addf(ctx, "unreachable", std_unreachable, 0);
	std_addf(ctx, "bitShiftLeft", std_bit_shift_left, 2);
	std_addf(ctx, "bitShiftRight", std_bit_shift_right, 2);
    std_addf(ctx, "bitAnd", std_bit_and, 2);
	std_addf(ctx, "bitOr", std_bit_or, 2);
	std_addf(ctx, "bitXor", std_bit_xor, 2);
	std_addf(ctx, "bitNot", std_bit_not, 1);
	std_addlib(ctx);
	std_addlib(ctx);

	/* system */
	std_setlib(ctx, "System");
	std_add(ctx, "version",	wrapstring(string_lit(SYLT_VERSION_STR, ctx)));
	std_add(ctx, "platform", wrapstring(string_lit(get_platform(), ctx)));
	std_addf(ctx, "memInfo", stdsys_mem_info, 0);
	std_addf(ctx, "memSizes", stdsys_mem_sizes, 0);
	std_addf(ctx, "src", stdsys_src, 0);
	std_addf(ctx, "exec", stdsys_exec, 1);
	std_addf(ctx, "halt", stdsys_halt, 1);
	std_addf(ctx, "timeFormat", stdsys_time_format, 1);
	std_addf(ctx, "timeStamp", stdsys_time_stamp, 0);
	std_addf(ctx, "cpuClock", stdsys_cpu_clock, 0);
	std_addlib(ctx);
		
	/* list */
	std_setlib(ctx, "List");
	std_addf(ctx, "init", stdlist_init, 2);
	std_addf(ctx, "get", stdlist_get, 2);
	std_addf(ctx, "set", stdlist_set, 3);
	std_addf(ctx, "swap", stdlist_swap, 3);
	std_addf(ctx, "add", stdlist_add, 3);
	std_addf(ctx, "push", stdlist_push, 2);
	std_addf(ctx, "del", stdlist_del, 2);
	std_addf(ctx, "pop", stdlist_pop, 1);
	std_addf(ctx, "count", stdlist_count, 2);
	std_addf(ctx, "contains", stdlist_contains, 2);
	std_addf(ctx, "find", stdlist_find, 2);
	std_addf(ctx, "rev", stdlist_rev, 1);
	std_addf(ctx, "range", stdlist_range, 2);
	std_addlib(ctx);
	
	/* dict */
	std_setlib(ctx, "Dict");
	std_addf(ctx, "get", stddict_get, 2);
	std_addf(ctx, "set", stddict_set, 3);
	std_addf(ctx, "hasKey", stddict_has_key, 2);
	std_addf(ctx, "keys", stddict_keys, 1);
	std_addf(ctx, "values", stddict_values, 1);
	std_addlib(ctx);
	
	/* string */
	std_setlib(ctx, "String");
	std_add(ctx, "letters", wrapstring(string_lit(
		"abcdefghijklmnopqrstuvwxyz",  ctx)));
	std_add(ctx, "digits", wrapstring(string_lit(
		"0123456789", ctx)));
	std_add(ctx, "punctuation", wrapstring(string_lit(
		"!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~", ctx)));
	std_add(ctx, "whitespace", wrapstring(string_lit(
		" \n\r\t", ctx)));
	std_addf(ctx, "chars", stdstring_chars, 1);
	std_addf(ctx, "ascii", stdstring_ascii, 1);
	std_addf(ctx, "join", stdstring_join, 1);
	std_addf(ctx, "split", stdstring_split, 2);
	std_addf(ctx, "lowercase", stdstring_lowercase, 1);
	std_addf(ctx, "uppercase", stdstring_uppercase, 1);
	std_addf(ctx, "swapcase", stdstring_swapcase, 1);
	std_addf(ctx, "isLower", stdstring_is_lower, 1);
	std_addf(ctx, "isUpper", stdstring_is_upper, 1);
	std_addf(ctx, "isWhitespace", stdstring_is_whitespace, 1);
	std_addf(ctx, "startsWith", stdstring_starts_with, 2);
	std_addf(ctx, "endsWith", stdstring_ends_with, 2);
	std_addf(ctx, "trimStart", stdstring_trim_start, 1);
	std_addf(ctx, "trimEnd", stdstring_trim_end, 1);
	std_addf(ctx, "trim", stdstring_trim, 1);
	std_addf(ctx, "find", stdstring_find, 3);
	std_addf(ctx, "replace", stdstring_replace, 3);
	std_addlib(ctx);
		
	/* file */
	std_setlib(ctx, "File");
	std_addf(ctx, "open", stdfile_open, 2);
	std_addf(ctx, "close", stdfile_close, 1);
	std_addf(ctx, "read", stdfile_read, 1);
	std_addf(ctx, "write", stdfile_write, 2);
	std_addf(ctx, "size", stdfile_size, 1);
	std_addf(ctx, "listDir", stdfile_list_dir, 1);
	std_addf(ctx, "del", stdfile_del, 1);
	std_addlib(ctx);
	
	/* math */
	std_setlib(ctx, "Math");
	std_add(ctx, "pi", wrapnum(M_PI));
	std_add(ctx, "e", wrapnum(M_E));
	std_addf(ctx, "abs", stdmath_abs, 1);
	std_addf(ctx, "log", stdmath_log, 2);
	std_addf(ctx, "pow", stdmath_pow, 2);
	std_addf(ctx, "sqrt", stdmath_sqrt, 1);
	std_addf(ctx, "floor", stdmath_floor, 1);
	std_addf(ctx, "ceil", stdmath_ceil, 1);
	std_addf(ctx, "round", stdmath_round, 1);
	std_addf(ctx, "rad", stdmath_rad, 1);
	std_addf(ctx, "deg", stdmath_deg, 1);
	std_addf(ctx, "sin", stdmath_sin, 1);
	std_addf(ctx, "cos", stdmath_cos, 1);
	std_addf(ctx, "tan", stdmath_tan, 1);
	std_addf(ctx, "asin", stdmath_asin, 1);
	std_addf(ctx, "acos", stdmath_acos, 1);
	std_addf(ctx, "atan", stdmath_atan, 1);
	std_addf(ctx, "sinh", stdmath_sinh, 1);
	std_addf(ctx, "cosh", stdmath_cosh, 1);
	std_addf(ctx, "tanh", stdmath_tanh, 1);
	std_addf(ctx, "asinh", stdmath_asinh, 1);
	std_addf(ctx, "acosh", stdmath_acosh, 1);
	std_addf(ctx, "atanh", stdmath_atanh, 1);
	std_addlib(ctx);

	/* vec3 */
	std_setlib(ctx, "Vec3");
	std_addlib(ctx);

	/* rand */
	std_setlib(ctx, "Rand");
	std_addf(ctx, "range", stdrand_range, 2);
	std_addf(ctx, "seed", stdrand_seed, 1);
	std_addlib(ctx);

	/* part of the stdlib is implemented 
	 * in sylt */
	sylt_xfile(ctx, SYLT_STDLIB_PATH);
	
	gc_resume(ctx);
}

void std_sandbox(sylt_t* ctx) {
	sylt_dprintf("Running in sandbox mode; File API and OS.exec() are disabled\n");

	/* disable entire File API */
	dict_set(ctx->vm->gdict, string_lit("File", ctx), unit(), ctx);

	/* disable OS.exec */
	dict_t* system = getdict(*dict_get(ctx->vm->gdict, string_lit("OS", ctx)));
	dict_set(system, string_lit("exec", ctx), unit(), ctx);
}

/* == compiler == */

typedef enum {
	T_NAME,
	T_NULL,
	T_TRUE,
	T_FALSE,
	T_LET,
	T_FUN,
	T_IF,
    T_THEN,
	T_ELSE,
	T_WHILE,
	T_AND,
	T_OR,
	T_IS,
	T_NOT,
	T_USING,
	T_RETURN,
	T_DO,
	T_END,
    T_MODULE,
	T_STRING,
	T_NUMBER,
	T_PLUS,
	T_PLUS_EQ,
	T_MINUS,
	T_MINUS_GT,
	T_MINUS_EQ,
	T_STAR,
	T_STAR_EQ,
	T_SLASH,
	T_SLASH_EQ,
	T_PERCENT,
	T_PERCENT_EQ,
	T_LT,
	T_LT_EQ,
	T_LT_MINUS,
	T_GT,
	T_GT_EQ,
	T_EQ,
	T_LPAREN,
	T_RPAREN,
	T_LCURLY,
	T_RCURLY,
	T_LSQUARE,
	T_RSQUARE,
	T_COMMA,
	T_COLON,
    T_SEMICOLON,
	T_DOT,
	T_HASH,
	T_EOF,
} token_type_t;

/* the source code is scanned into a series of
 * tokens */
typedef struct {
	token_type_t tag;
	string_t* lex;
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
	/* boolean 'not' in infix position */
	PREC_NOT,
	/* equality: is is not */
	PREC_EQ,
	/* comparison: < <= > >= */
	PREC_CMP,
	/* addition: + - */
	PREC_TERM,
	/* multiplication: * / % */
	PREC_FACTOR,
	/* unary prefix: - not # */
	PREC_UNARY_PREFIX,
	/* unary postfix: . */
	PREC_DOT,
	/* function application: : () */
	PREC_FUNC_CALL,
	/* unary minus: - */
	PREC_UNARY_MINUS,
	/* unused */
	PREC_PRIM,
} prec_t;

const int ANY_PREC = PREC_ASSIGN;

typedef struct {
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
	/* files visited with 'using' */
	list_t* included;
	/* name of current module */
	string_t* module;
	/* reference to API */
	sylt_t* ctx;
} comp_t;

void comp_init(comp_t* cmp, comp_t* parent, comp_t* child, sylt_t* ctx) {
	cmp->parent = parent;
	cmp->child = child;
	cmp->func = NULL;
	cmp->curslots = 0;
	
	cmp->pos = NULL;
	cmp->line = 1;
	cmp->syms = NULL;
	cmp->nsyms = 0;
	cmp->depth = 0;
	cmp->included = list_new(ctx);
	cmp->module = NULL;
	cmp->ctx = ctx;
}

void comp_free(comp_t* cmp) {
	arr_free(cmp->syms, symbol_t, cmp->nsyms, cmp->ctx);
}

void comp_load(comp_t* cmp, string_t* src, string_t* name) {
	cmp->func = func_new(cmp->ctx, name, src);
	if (cmp->parent) {
		/* parent_name/ */
		sylt_pushstring(cmp->ctx, cmp->parent->func->path);
		sylt_pushstring(cmp->ctx, string_lit("/", cmp->ctx));
		sylt_concat(cmp->ctx);
		
		/* parent_name/child_name */
		sylt_pushstring(cmp->ctx, name);
		sylt_concat(cmp->ctx);
		
		cmp->func->path = sylt_popstring(cmp->ctx);
	}
	
	cmp->pos = (char*)cmp->func->src->bytes;
	cmp->line = 1;
	
	if (list_count(cmp->included,
		wrapstring(name)) == 0)
		list_push(cmp->included, wrapstring(name), cmp->ctx);
}

void comp_copy_parse_state(comp_t* dst, const comp_t* src) {
	dst->pos = src->pos;
	dst->line = src->line;
	dst->prev = src->prev;
	dst->cur = src->cur;
}

/* == codegen == */

void comp_simstack(comp_t* cmp, int n) {
	cmp->curslots += n;
	
	/* TODO: uncomment */
	//assert(cmp->curslots >= 0);
	
	if (cmp->curslots > MAX_STACK) {
		halt(cmp->ctx, E_STACK_LIMIT);
		unreachable();
	}
	
	/* record the largest stack size */
	if (cmp->curslots > (int)cmp->func->slots)
		cmp->func->slots = cmp->curslots;
}

/* should not be used directly; use the
 * emit_ functions below instead */
void emit_op(comp_t* cmp, op_t op, const uint8_t* args, size_t nargs) {
	comp_simstack(cmp, OPINFO[op].effect);
	
	/* write the instruction opcode */
	func_write(cmp->func, op, cmp->prev.line, cmp->ctx);
	
	/* write the arguments */
	for (size_t i = 0; i < nargs; i++)
		func_write(cmp->func, args[i], cmp->prev.line, cmp->ctx);
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
	
	switch (val.tag) {
	case TYPE_UNIT: {
		emit_nullary(cmp, OP_PUSH_NULL);
		sylt_pop(cmp->ctx); /* GC */
		return;
	}
	case TYPE_BOOL: {
		emit_nullary(cmp, (getbool(val)) ? OP_PUSH_TRUE : OP_PUSH_FALSE);
		sylt_pop(cmp->ctx); /* GC */
		return;
	}
	default: break;
	}
	
	size_t slot = func_write_data(cmp->func, val, cmp->ctx);
	if (val.tag == TYPE_FUNCTION)
		emit_unary(cmp, OP_PUSH_FUNC, slot);
	else
		emit_unary(cmp, OP_PUSH, slot);

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
	int dist = cmp->func->ncode - addr - 2;
	if (dist > MAX_JUMP) {
		halt(cmp->ctx, E_JUMP_LIMIT);
		return;
	}
	
	/* encode short as two bytes */
	cmp->func->code[addr] = (dist >> 8) & 0xff;
	cmp->func->code[addr + 1] = dist & 0xff;
}

void emit_loop(comp_t* cmp, int addr) {
	func_write(cmp->func, OP_JMP_BACK, cmp->prev.line, cmp->ctx);
	
	int dist = cmp->func->ncode - addr + 2;
	if (dist > MAX_JUMP) {
		halt(cmp->ctx, E_JUMP_LIMIT);
		return;
	}
	
	func_write(cmp->func, (dist >> 8) & 0xff, cmp->prev.line, cmp->ctx);
	func_write(cmp->func, dist & 0xff, cmp->prev.line, cmp->ctx);
}

/* adds a name to the symbol table
 * and returns its index */
int add_symbol(comp_t* cmp, string_t* name) {
	symbol_t sym;
	sym.name = name;
	sym.depth = cmp->depth;
	sym.capped = false;
	
	cmp->syms = arr_resize(cmp->syms, symbol_t, cmp->nsyms, cmp->nsyms + 1, cmp->ctx);
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
int add_upvalue(comp_t* cmp, uint8_t index, bool is_local) {
	size_t n = cmp->func->upvalues;
	
	/* check if one already exists */
	for (size_t i = 0; i < n; i++) {
		cmp_upvalue_t upval = cmp->upvals[i];
		if (upval.index == index && upval.is_local == is_local)
			return i;
		
	}
	
	if (n == MAX_UPVALUES) {
		halt(cmp->ctx, E_TOO_MANY_UPVALUES);
		unreachable();
	}
	
	cmp->upvals[n].is_local = is_local;
	cmp->upvals[n].index = index;
	return cmp->func->upvalues++;
}

/* returns the index of an upvalue in the
 * upvalue array or -1 if not found */
int find_upvalue(comp_t* cmp, string_t* name) {
	if (!cmp->parent)
		return -1;
	
	/* search for a local variable in the
	 * enclosing function */
	int local = find_symbol(cmp->parent, name);
	if (local != -1) {
		cmp->parent->syms[local].capped = true;
		return add_upvalue(cmp, local, true);
	}
	
	/* recursively search for an upvalue
	 * higher up */
	int upvalue = find_upvalue(cmp->parent, name);
	if (upvalue != -1)
		return add_upvalue(cmp, upvalue, false);
	
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
	while (index >= 0 && cmp->syms[index].depth > cmp->depth) {
		locals++;
		index--;
	}
	
	if (locals == 0)
		return;
		
	/* the return value is on top of the
	 * stack so we need to hide it */
	emit_nullary(cmp, OP_STORE_RET);
	
	/* pop the locals */
	while (cmp->nsyms > 0 && cmp->syms[cmp->nsyms - 1].depth > cmp->depth) {
		symbol_t* name = &cmp->syms[cmp->nsyms - 1];
		cmp->nsyms--;
		
		if (name->capped)
			emit_nullary(cmp, OP_POP_HEAP);
		else
			emit_nullary(cmp, OP_POP);
	}
	
	/* shrink symbol table */
	cmp->syms = arr_resize(cmp->syms, symbol_t, cmp->nsyms + locals, cmp->nsyms, cmp->ctx);
		
	/* restore the return value */
	emit_nullary(cmp, OP_LOAD_RET);
}

/* == lexer == */

#define token(tag) \
	(token_t){ \
		tag, \
		string_new((uint8_t*)start, cmp->pos - start, cmp->ctx), \
		cmp->line}
#define step() cmp->pos++
#define peek() (*cmp->pos)
#define peek_next() \
	(eof() ? '\0' : cmp->pos[1])
#define is(c) (peek() == (c))
#define next_is(c) (peek_next() == (c))
#define eof() is('\0')
#define match(c) \
	((!eof() && is(c)) ? step(), true : false)

/* scans the source code for the next token */
token_t scan(comp_t* cmp) {
	/* skip over any whitespace */
	while (!eof()) {
		if (!isspace(peek())) {
			/* single-line comment */
			if (is('#') && next_is('#'))
				while (!is('\n') && !eof())
					step();
				
			else
				break;
		}
		
		if (is('\n'))
			cmp->line++;
		
		step();
	}
	
	/* remember first non-whitespace char */
	const char* start = cmp->pos;
	if (eof()) goto return_eof;
	
	/* symbol name or keyword */
	if (isalpha(peek()) || is('_')) {
		while (isalnum(peek()) || is('_') || is('/')) step();
		
		size_t len = cmp->pos - start;
		#define keyword(lit) (len == strlen(lit) && !strncmp(start, (lit), strlen(lit)))

		if (keyword("null")) return token(T_NULL);
		if (keyword("true")) return token(T_TRUE);
		if (keyword("false")) return token(T_FALSE);
		if (keyword("let")) return token(T_LET);
		if (keyword("fun")) return token(T_FUN);
		if (keyword("if")) return token(T_IF);
		if (keyword("then")) return token(T_THEN);
		if (keyword("else")) return token(T_ELSE);
		if (keyword("while")) return token(T_WHILE);
		if (keyword("and")) return token(T_AND);
		if (keyword("or")) return token(T_OR);
		if (keyword("is")) return token(T_IS);
		if (keyword("not")) return token(T_NOT);
		if (keyword("using")) return token(T_USING);
		if (keyword("return")) return token(T_RETURN);
		if (keyword("do")) return token(T_DO);
		if (keyword("end")) return token(T_END);
		if (keyword("module")) return token(T_MODULE);
			
		#undef keyword
		
		return token(T_NAME);
	}
	
	/* string literal */
	if (is('"')) {
		step();
		while (!is('"') && !eof()) {
			step();

			bool ignore_close = is('"')
				&& cmp->pos - start >= 2
				&& (cmp->pos[-1] == '\\'
					&& cmp->pos[-2] != '\\');
					
			if (ignore_close)
				step();
		}
		
		if (eof())
			halt(cmp->ctx, E_UNTERM_STRING);
		
		step();
		return token(T_STRING);
	}
	
	/* number */
	if (isdigit(peek())) {
		while (isdigit(peek()))
			step();
		
		if (is('.') && isdigit(peek_next())) {
			step();
			while (isdigit(peek()))
				step();
		}
		
		return token(T_NUMBER);
	}
	
	switch (*step()) {
	case '+':
		if (match('=')) return token(T_PLUS_EQ);
		return token(T_PLUS);
	case '-':
		if (match('>')) return token(T_MINUS_GT);
		if (match('=')) return token(T_MINUS_EQ);
		return token(T_MINUS);
	case '*':
		if (match('=')) return token(T_STAR_EQ);
		return token(T_STAR);
	case '/':
		if (match('=')) return token(T_SLASH_EQ);
		return token(T_SLASH);
	case '%':
		if (match('=')) return token(T_PERCENT_EQ);
		return token(T_PERCENT);
	case '<':
		if (match('=')) return token(T_LT_EQ);
		if (match('-')) return token(T_LT_MINUS);
		return token(T_LT);
	case '>':
		if (match('=')) return token(T_GT_EQ);
		return token(T_GT);
	case '=': return token(T_EQ);
	case '(': return token(T_LPAREN);
	case ')': return token(T_RPAREN);
	case '{': return token(T_LCURLY);
	case '}': return token(T_RCURLY);
	case '[': return token(T_LSQUARE);
	case ']': return token(T_RSQUARE);
	case ',': return token(T_COMMA);
	case ':': return token(T_COLON);
	case ';': return token(T_SEMICOLON);
	case '.': return token(T_DOT);
	case '#': return token(T_HASH);
	case '\0': break;
	default: halt(cmp->ctx, E_UNEXPECTED_CHAR(cmp->pos[-1]));
	}

	return_eof:
	token_t tok = token(T_EOF);
	tok.lex = string_lit("<end of file>", cmp->ctx);
	return tok;
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
void eat(comp_t* cmp, token_type_t tag, const char* msg) {
	if (check(cmp, tag)) {
		step(cmp);
		return;
	}
	
	halt(cmp->ctx, E_PARSER_EXPECTED(msg, cmp->prev.lex));
}

/* parsing functions */
void expr(comp_t*, prec_t, const char*);
void literal(comp_t*);
void name(comp_t*);
void list(comp_t*);
void dict(comp_t*);
void string(comp_t*);
void number(comp_t*);
void grouping(comp_t*);
void unary(comp_t*);
void binary(comp_t*);
void using(comp_t*);
void ret(comp_t*);
void let(comp_t*);
void fun(comp_t*);
void if_else(comp_t*);
void while_loop(comp_t*);
void block(comp_t*);
void module(comp_t*);

typedef void (*parsefn_t)(comp_t*);

/* all tokens match a parse rule */
typedef struct {
	parsefn_t prefix;
	parsefn_t infix;
	prec_t prec;
} parserule_t;

/* maps tokens to parsers */
static parserule_t RULES[] = {
	[T_NAME] = {name, NULL, PREC_NONE},
	[T_NULL] = {literal, NULL, PREC_NONE},
	[T_TRUE] = {literal, NULL, PREC_NONE},
	[T_FALSE] = {literal, NULL, PREC_NONE},
	[T_LET] = {let, NULL, PREC_NONE},
	[T_FUN] = {fun, NULL, PREC_NONE},
	[T_IF] = {if_else, NULL, PREC_NONE},
    [T_THEN] = {NULL, NULL, PREC_NONE},
	[T_ELSE] = {NULL, NULL, PREC_NONE},
	[T_WHILE] = {while_loop, NULL, PREC_NONE},
	[T_AND] = {NULL, binary, PREC_AND},
	[T_OR] = {NULL, binary, PREC_OR},
	[T_IS] = {NULL, binary, PREC_EQ},
	[T_NOT] = {unary, binary, PREC_NOT},
	[T_USING] = {using, NULL, PREC_NONE},
	[T_RETURN] = {ret, NULL, PREC_NONE},
	[T_DO] = {block, NULL, PREC_NONE},
	[T_END] = {NULL, NULL, PREC_NONE},
    [T_MODULE] = {module, NULL, PREC_NONE},
	[T_STRING] = {string, NULL, PREC_NONE},
	[T_NUMBER] = {number, NULL, PREC_NONE},
	[T_PLUS] = {NULL, binary, PREC_TERM},
	[T_PLUS_EQ] = {NULL, NULL, PREC_NONE},
	[T_MINUS] = {unary, binary, PREC_TERM},
	[T_MINUS_GT] = {NULL, NULL, PREC_NONE},
	[T_MINUS_EQ] = {NULL, NULL, PREC_NONE},
	[T_STAR] = {NULL, binary, PREC_FACTOR},
	[T_STAR_EQ] = {NULL, NULL, PREC_NONE},
	[T_SLASH] = {NULL, binary, PREC_FACTOR},
	[T_SLASH_EQ] = {NULL, NULL, PREC_NONE},
	[T_PERCENT] = {NULL, binary, PREC_FACTOR},
	[T_PERCENT_EQ] = {NULL, NULL, PREC_NONE},
	[T_LT] = {NULL, binary, PREC_CMP},
	[T_LT_EQ] = {NULL, binary, PREC_CMP},
	[T_LT_MINUS] = {NULL, NULL, PREC_NONE},
	[T_GT] = {NULL, binary, PREC_CMP},
	[T_GT_EQ] = {NULL, binary, PREC_CMP},
	[T_EQ] = {NULL, NULL, PREC_NONE},
	[T_LPAREN] = {grouping, binary, PREC_FUNC_CALL},
	[T_RPAREN] = {NULL, NULL, PREC_NONE},
	[T_LCURLY] = {dict, NULL, PREC_NONE},
	[T_RCURLY] = {NULL, NULL, PREC_NONE},
	[T_LSQUARE] = {list, NULL, PREC_NONE},
	[T_RSQUARE] = {NULL, NULL, PREC_NONE},
	[T_COMMA] = {NULL, NULL, PREC_NONE},
	[T_COLON] = {NULL, binary, PREC_FUNC_CALL},
    [T_SEMICOLON] = {NULL, NULL, PREC_NONE},
	[T_DOT] = {NULL, binary, PREC_DOT},
	[T_HASH] = {unary, NULL, PREC_NONE},
	[T_EOF] = {NULL, NULL, PREC_NONE},
};

/* parses an expression at or above
 * the given precedence level */
void expr(comp_t* cmp, prec_t prec, const char* name) {
	/* move to the next token */
	step(cmp);
	parsefn_t prefix = RULES[cmp->prev.tag].prefix;
	
	/* first token in an expression
	 * must have a prefix rule */
	if (!prefix) {
		halt(cmp->ctx, "expected %s, got '%.*s'",
			name, (int)cmp->prev.lex->len, cmp->prev.lex->bytes);
		return;
	}
	
	/* parse the rest of the expression */
	prefix(cmp);
	while (prec <= RULES[cmp->cur.tag].prec) {
		step(cmp);
		parsefn_t infix = RULES[cmp->prev.tag].infix;
		infix(cmp);
	}
}

/* parses a keyword literal */
void literal(comp_t* cmp) {
	switch (cmp->prev.tag) {
	case T_NULL: emit_value(cmp, unit()); break;
	case T_TRUE: emit_value(cmp, wrapbool(true)); break;
	case T_FALSE: emit_value(cmp, wrapbool(false)); break;
	default: unreachable();
	}
}

void load_name(comp_t* cmp, string_t* name) {
	/* check if the name is in this functions
	 * symbol table */
	int index = find_symbol(cmp, name);
	if (index != -1) {
		emit_unary(cmp, OP_LOAD, index);
		return;
	}
	
	/* check if the name is in an upvalue */
	index = find_upvalue(cmp, name);
	if (index != -1) {
		emit_unary(cmp, OP_LOAD_UPVAL, index);
		return;
	}
	
	/* runtime lookup */
	index = func_write_data(cmp->func, wrapstring(name), cmp->ctx);
	emit_unary(cmp, OP_LOAD_NAME, index);
}

void store_name(comp_t* cmp, string_t* name) {
	int index = find_symbol(cmp, name);
	if (index != -1) {
		emit_unary(cmp, OP_STORE, index);
		return;
	}
	
	index = find_upvalue(cmp, name);
	if (index != -1) {
		emit_unary(cmp, OP_STORE_UPVAL, index);
		return;
	}
	
	index = func_write_data(cmp->func, wrapstring(name), cmp->ctx);
	emit_unary(cmp, OP_STORE_NAME, index);
}

/* parses a symbol name */
void name(comp_t* cmp) {
	string_t* name = cmp->prev.lex;
	
	/* figure out if we're storing or 
	 * loading a variable */
	bool assign = false;
	bool is_compound = match(cmp, T_PLUS_EQ)
		|| match(cmp, T_MINUS_EQ)
		|| match(cmp, T_STAR_EQ)
		|| match(cmp, T_SLASH_EQ)
		|| match(cmp, T_PERCENT_EQ);
	
	if (match(cmp, T_LT_MINUS) || is_compound) {
		token_type_t op = cmp->prev.tag;
		if (is_compound)
			load_name(cmp, name);

		assign = true;
		expr(cmp, PREC_ASSIGN, "expression");

		if (is_compound) {
			switch (op) {
			case T_PLUS_EQ: emit_nullary(cmp, OP_ADD); break;
			case T_MINUS_EQ: emit_nullary(cmp, OP_SUB); break;
			case T_STAR_EQ: emit_nullary(cmp, OP_MUL); break;
			case T_SLASH_EQ: emit_nullary(cmp, OP_DIV); break;
			case T_PERCENT_EQ: emit_nullary(cmp, OP_EDIV); break;
			default: unreachable();
			}
		}
	}
	
	if (assign)
		store_name(cmp, name);
	else
		load_name(cmp, name);
}

/* parses a list literal */
void list(comp_t* cmp) {
	int len = 0;
	while (!check(cmp, T_RSQUARE) && !check(cmp, T_EOF)) {
		expr(cmp, PREC_OR, "a list item, or ']'");
		len++;
	}
	
	eat(cmp, T_RSQUARE, "unterminated list, expected ']'");
	emit_unary(cmp, OP_PUSH_LIST, len);
}

/* parses a dictionary literal */
void dict(comp_t* cmp) {
	int len = 0;
	while (!check(cmp, T_RCURLY) && !check(cmp, T_EOF)) {
		expr(cmp, PREC_OR, "a dictionary key or ']'"); /* key */
		eat(cmp, T_EQ, "expected '=' after item key");
		expr(cmp, PREC_OR, "key value"); /* value */
		len++;
	}
	
	eat(cmp, T_RCURLY, "unterminated dict, expected '}'");
	emit_unary(cmp, OP_PUSH_DICT, len * 2);
}

/* parses a string literal */
void string(comp_t* cmp) {
	token_t token = cmp->prev;
	assert(token.lex->len >= 2); /* "" */
	
	/* allocate an empty string the same size
	 * as the length of the string literal */
	string_t* dst = string_new(NULL, token.lex->len - 2, cmp->ctx);
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
		size_t seqlen =
			(code == 'x') ? 4 : 2;
			
		switch (code) {
		case 'x': {
			/* make sure we have two digits */
			if (end - read < seqlen) {
				size_t needed = seqlen - (end - read);
			
				halt(cmp->ctx, "expected %ld more character%s after \\%c",
					needed, (needed > 1) ? "s" : "", code);
			}
			
			/* parse hexadecimal byte */
			char* start = (char*)src + read + 2;
			char* end = start + 2;
			unsigned long byte = strtoul(start, &end, 16);
			
			/* parsing failed */
			if (end < start + 2)
				halt(cmp->ctx, "invalid character in \\x escape: '%c'", *end);
				
			dst->bytes[write++] = byte;
			break;
		}
		case '\\': dst->bytes[write++] = '\\'; break;
		case '\"': dst->bytes[write++] = '\"'; break;
		case 't': dst->bytes[write++] = '\t'; break;
		case 'n': dst->bytes[write++] = '\n'; break;
		case 'r': dst->bytes[write++] = '\r'; break;
		case '0': dst->bytes[write++] = '\0'; break;
		default: halt(cmp->ctx, E_ESCAPE_SEQ(code));
		}
			
		read += seqlen;
	}
	
	/* shrink to fit */
	if (write < dst->len) {
		dst->bytes = arr_resize(dst->bytes, uint8_t, dst->len, write, cmp->ctx);
		dst->len = write;
	}
	
	string_rehash(dst, cmp->ctx);
	emit_value(cmp, wrapstring(dst));
}

/* parses a numeric literal */
void number(comp_t* cmp) {
	sylt_num_t num = num_func(strtof, strtod)((char*)cmp->prev.lex->bytes, NULL);
	
	emit_value(cmp, wrapnum(num));
}

/* parses a parenthesized expression */
void grouping(comp_t* cmp) {
	expr(cmp, ANY_PREC, "expression");
	eat(cmp, T_RPAREN, "expected closing ')'");
}

/* parses a unary expression */
void unary(comp_t* cmp) {
	/* save the operator token */
	token_type_t token = cmp->prev.tag;
	
	/* parse the operand */
	if (token == T_MINUS)
		expr(cmp, PREC_UNARY_PREFIX, "expression to negate");
	else
		expr(cmp, PREC_UNARY_PREFIX, "right-hand side expression");
	
	op_t opcode = -1;
	switch (token) {
	case T_MINUS: opcode = OP_UMIN; break;
	case T_NOT: opcode = OP_NOT; break;
	case T_HASH: opcode = OP_LENGTH; break;
	default: unreachable();
	}
	
	emit_nullary(cmp, opcode);
}

/* parses a binary expression */
void binary(comp_t* cmp) {
	/* left hand expression has already been
	 * parsed along with the operator token */
	token_type_t token = cmp->prev.tag;
	
	/* compile the right hand expression */
	parserule_t* rule = &RULES[token];
	
	op_t opcode = -1;
	switch (token) {
	case T_IS: opcode = OP_EQ; break;
	case T_NOT: opcode = OP_NEQ; break;
	/* arithmetic */
	case T_PLUS: opcode = OP_ADD; break;
	case T_MINUS: opcode = OP_SUB; break;
	case T_STAR: opcode = OP_MUL; break;
	case T_SLASH: opcode = OP_DIV; break;
	case T_PERCENT: opcode = OP_EDIV; break;
	/* comparison */
	case T_LT: opcode = OP_LT; break;
	case T_LT_EQ: opcode = OP_LTE; break;
	case T_GT: opcode = OP_GT; break;
	case T_GT_EQ: opcode = OP_GTE; break;
	/* control flow */
	case T_AND: {
		/* if the left-hand side expression
		 * is false we jump past the 
		 * right-hand side expression */
		int jump = emit_jump(cmp, OP_JMP_IF_NOT);
		
		emit_nullary(cmp, OP_POP);
		expr(cmp, PREC_AND, "expression");
		
		patch_jump(cmp, jump);
		return; /* early return */
	}
	case T_OR: {
		/* if the left-hand side expression
		 * is true we jump past the 
		 * right-hand side expression */
		int jump = emit_jump(cmp, OP_JMP_IF);
		
		emit_nullary(cmp, OP_POP);
		expr(cmp, PREC_OR, "expression");
		
		patch_jump(cmp, jump);
		return; /* early return */
	}
	/* other special cases */
	case T_LPAREN: {
		int argc = 0;
	
		/* parse argument list */
		while (!check(cmp, T_RPAREN) && !check(cmp, T_EOF)) {
			if (argc >= MAX_PARAMS) {
				halt(cmp->ctx, E_TOO_MANY_ARGS);
				unreachable();
			}
				
			expr(cmp, ANY_PREC, "argument or ')'");
			argc++;
		}
		
		eat(cmp, T_RPAREN, "expected ')' after call arguments");
		emit_unary(cmp, OP_CALL, argc);
		comp_simstack(cmp, -argc);
		return;
	}
	case T_COLON: {
        int argc = 0;
	
		/* parse argument list */
		while (!check(cmp, T_SEMICOLON) && !check(cmp, T_EOF)) {
			if (argc >= MAX_PARAMS) {
				halt(cmp->ctx, E_TOO_MANY_ARGS);
				unreachable();
			}
				
			expr(cmp, ANY_PREC, "argument or ';'");
			argc++;
		}
		
		eat(cmp, T_SEMICOLON, "expected ';' after call arguments");
		emit_unary(cmp, OP_CALL, argc);
		comp_simstack(cmp, -argc);
		return;
	}
	case T_DOT: {
		eat(cmp, T_NAME, "expected member name after '.'");
		string_t* name = cmp->prev.lex;
		emit_value(cmp, wrapstring(name));

		if (match(cmp, T_LT_MINUS)) {
			expr(cmp, PREC_ASSIGN, "right-hand side expression");
			emit_nullary(cmp, OP_STORE_KEY);
			return;
		}

		emit_nullary(cmp, OP_LOAD_KEY);
		return;
	}
	default: unreachable();
	}
	
	expr(cmp, rule->prec + 1, "right-hand side expression");
	emit_nullary(cmp, opcode);
}

string_t* load_file(const char*, sylt_t*);
void compile_and_run(sylt_t*, string_t*, string_t*, comp_t*);

/* parses a using expression */
void using(comp_t* cmp) {
	eat(cmp, T_NAME, "expected file name after 'using'");
	
	string_t* ext = string_lit(".sylt", cmp->ctx);
	
	/* no extension looks cleaner */
	if (string_ends_with(cmp->prev.lex, ext))
		halt(cmp->ctx, E_SYLT_EXTENSION);
	
	/* append extension */
	sylt_pushstring(cmp->ctx, cmp->prev.lex);
	sylt_pushstring(cmp->ctx, ext);
	sylt_concat(cmp->ctx);
	string_t* path = sylt_popstring(cmp->ctx);
	
	/* prevent infinite loops */
	if (list_count(cmp->included, wrapstring(path)) > 0)
		return;
	list_push(cmp->included, wrapstring(path), cmp->ctx);
	
	/* compile using a separate compiler */
	comp_t import;
	comp_init(&import, NULL, NULL, cmp->ctx);
	import.included = cmp->included;
	
	compile_and_run(cmp->ctx,
		load_file((const char*)path->bytes, cmp->ctx),
		path,
		&import);
}

/* parses an early return */
void ret(comp_t* cmp) {
	expr(cmp, ANY_PREC, "return value");
	emit_nullary(cmp, OP_RET);
}

void parse_func(comp_t*, string_t*);

/* parses a variable or function binding */
void let(comp_t* cmp) {
	eat(cmp, T_NAME, "expected variable name after 'let'");
	string_t* name = cmp->prev.lex;
	
	bool is_local = cmp->depth > 0;
	if (check(cmp, T_NAME)) {
		/* parse a function declaration
 		* in the form of
 		* let name param1 param2 .. = body */
 
		/* add symbol first in order
	 	* to support recursion */
		if (is_local)
			add_symbol(cmp, name);
		 
		parse_func(cmp, name);

		if (cmp->module) {
            emit_binary(cmp,
			    OP_ADD_MOD,
			    func_write_data(cmp->func, wrapstring(name), cmp->ctx),
			    func_write_data(cmp->func, wrapstring(cmp->module), cmp->ctx));
			
		} else if (!is_local) {
			int index = func_write_data(cmp->func, wrapstring(name), cmp->ctx);
			emit_unary(cmp, OP_ADD_NAME, index);
		}
	} else {
		/* parse a variable declaration 
		 * in the form of 
		 * let name = expr */
		eat(cmp, T_EQ, "expected '=' after variable name");
		
		/* compile the right hand side
	 	* of the expression */
		expr(cmp, ANY_PREC, "right-hand side expression after '='");
		
		if (is_local) {
			add_symbol(cmp, name);
		} else {
			int index = func_write_data(cmp->func, wrapstring(name), cmp->ctx);
			emit_unary(cmp, OP_ADD_NAME, index);
		}
	}
	
	/* expression yields '()' */
	emit_value(cmp, unit());
}

/* parses an anonymous function */
void fun(comp_t* cmp) {
	parse_func(cmp, NULL);
}

void parse_func(
	comp_t* cmp, string_t* name)
{
	bool is_lambda = false;
	if (!name) {
		name = string_lit("_", cmp->ctx);
		is_lambda = true;
	}
	
	/* setup a new compiler instance
	* in order to parse the function */
	comp_t fcmp;
	comp_init(&fcmp, cmp, NULL, cmp->ctx);
	comp_load(&fcmp, cmp->func->src, name);
	comp_copy_parse_state(&fcmp, cmp);
	fcmp.depth = cmp->depth;
	cmp->child = &fcmp;
	
	comp_open_scope(cmp);
		
	/* parse parameter list */
	while (!check(&fcmp, T_EQ) && !check(&fcmp, T_MINUS_GT) && !check(&fcmp, T_EOF)) {
		if (fcmp.func->params >= MAX_PARAMS) {
			halt(fcmp.ctx, E_TOO_MANY_PARAMS);
			unreachable();
		}
			
		eat(&fcmp, T_NAME, "expected parameter name");
		add_symbol(&fcmp, fcmp.prev.lex);
		fcmp.func->params++;
	}
	
	if (is_lambda) {
		eat(&fcmp, T_MINUS_GT, "expected '->' after ')'");
	} else {
		eat(&fcmp, T_EQ, "expected '=' after ')'");
	}
			
	/* function body */
	expr(&fcmp, ANY_PREC, "function body expression");
	
	comp_close_scope(cmp);
	emit_nullary(&fcmp, OP_RET);
	
	func_t* func = fcmp.func;
	emit_value(cmp, wrapfunc(func));
	
	/* write arguments to OP_PUSHFUNC */
	for (int i = 0; i < func->upvalues; i++) {
		cmp_upvalue_t* upval =
			&fcmp.upvals[i];
			
		func_write(cmp->func, upval->is_local, cmp->prev.line, cmp->ctx);
		func_write(cmp->func, upval->index, cmp->prev.line, cmp->ctx);
	}
	
	comp_copy_parse_state(cmp, &fcmp);
	comp_free(&fcmp);
	cmp->child = NULL;
}

/* parses an if/else expression */
void if_else(comp_t* cmp) {
	expr(cmp, ANY_PREC, "if condition"); /* condition */
	eat(cmp, T_THEN, "expected 'then' after if condition");
	
	/* jump to else branch if
	 * the condition is false */
	int then_addr = emit_jump(cmp, OP_JMP_IF_NOT);
		
	/* 'then' branch */
	emit_nullary(cmp, OP_POP); /* condition */
	expr(cmp, ANY_PREC, "'if' branch expression");
	
	/* unconditionally jump over the 
	 * else branch */
	int else_addr = emit_jump(cmp, OP_JMP);
	patch_jump(cmp, then_addr);
	
	/* 'else' branch */
	emit_nullary(cmp, OP_POP); /* condition */
	if (match(cmp, T_ELSE))
		expr(cmp, ANY_PREC, "'else' branch expression");
	else
		emit_value(cmp, unit());
	
	/* skipped past else */
	patch_jump(cmp, else_addr);
}

void while_loop(comp_t* cmp) {
	int loop_start = cmp->func->ncode;
	expr(cmp, ANY_PREC, "'while' condition"); /* condition */
		
	int jmp = emit_jump(cmp, OP_JMP_IF_NOT);
	emit_nullary(cmp, OP_POP);
	
	expr(cmp, ANY_PREC, "'while' body");
	emit_nullary(cmp, OP_POP);
	emit_loop(cmp, loop_start);
	
	patch_jump(cmp, jmp);
	emit_nullary(cmp, OP_POP);
	
	emit_value(cmp, unit());
}

/* parses a block expression.
 * in essence, a block is a series of
 * expressions ultimately reduced down
 * to a single value */
void block(comp_t* cmp) {
	comp_open_scope(cmp);
	
	/* empty block yields '()' */
	if (match(cmp, T_END)) {
		emit_value(cmp, unit());
		comp_close_scope(cmp);
		return;
	}
	
	while (!check(cmp, T_END) && !check(cmp, T_EOF)) {
		expr(cmp, ANY_PREC, "expression");
		
		/* pop the result of every expression
		 * in a block except for the last one,
		 * which becomes the return value
		 * of the entire block */
		if (!check(cmp, T_END))
			emit_nullary(cmp, OP_POP);
	}
	
	eat(cmp, T_END, "expected 'end'");
	comp_close_scope(cmp);
}

/* parses a module */
void module(comp_t* cmp) {
    eat(cmp, T_NAME, "expected module name");
	string_t* name = cmp->prev.lex;
    dict_set(cmp->vm->gdict, name, dict_new(), vm->ctx);
	
	eat(cmp, T_IS, "expected 'is' after module name");

	cmp->module = name;
	comp_open_scope(cmp);
	
    while (!check(cmp, T_END) && !check(cmp, T_EOF)) {
		expr(cmp, ANY_PREC, "expression");
		
		/* pop the result of every expression
		 * in a block except for the last one,
		 * which becomes the return value
		 * of the entire block */
		if (!check(cmp, T_END))
			emit_nullary(cmp, OP_POP);
	}
	
	eat(cmp, T_END, "expected 'end' to close module");
	comp_close_scope(cmp);
	cmp->module = NULL;
}

/* == GC == */

void gc_set_state(sylt_t* ctx, gc_state_t state) {
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
void gc_collect(sylt_t* ctx, const char* func_name, int line) {
	#if DBG_NO_GC
	return;
	#endif
	
	if (ctx->mem.gc.state != GC_STATE_IDLE || ctx->state != SYLT_STATE_EXEC)
		return;
	
	gc_mark(ctx, func_name, line);
	gc_sweep(ctx);
	gc_set_state(ctx, GC_STATE_IDLE);
	
	ctx->mem.gc.trigger = ctx->mem.bytes * GC_THRESHOLD_CLIMB;
	ctx->mem.gc.cycles++;
}

void gc_mark_vm(sylt_t*);
void gc_mark_compiler(sylt_t*);
void gc_trace_refs(sylt_t*);

/* marks all root objects */
void gc_mark(sylt_t* ctx, const char* func_name, int line) {
	#if DBG_PRINT_GC_STATE
	sylt_dprintf(
		"     gc @ %s:%d, ",
		func_name, line);
	#else
	(void)func_name, (void)line;
	#endif
	
	gc_set_state(ctx, GC_STATE_MARK);
	gc_mark_vm(ctx);
	gc_mark_compiler(ctx);
	gc_trace_refs(ctx);
	
	/* TODO */
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
	
	/* global variables */
	obj_mark((obj_t*)vm->gdict, ctx);
	
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
	if (!ctx->cmp)
		return;
	
	/* compiler stack */
	comp_t* cmp = ctx->cmp;
	do {
		obj_mark((obj_t*)cmp->func, ctx);
		obj_mark((obj_t*)cmp->func->src, ctx);
		obj_mark((obj_t*)cmp->prev.lex, ctx);
		obj_mark((obj_t*)cmp->cur.lex, ctx);
		for (size_t i = 0; i < cmp->nsyms; i++)
			obj_mark((obj_t*)cmp->syms[i].name, ctx);
		
		cmp = cmp->child;
	} while (cmp);
}

/* iterates through all marked roots
 * and marks any child objects reachable
 * from them */
void gc_trace_refs(sylt_t* ctx) {
	while (ctx->mem.gc.nmarked > 0) {
		obj_t* obj = ctx->mem.gc.marked[--ctx->mem.gc.nmarked];
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
	size_t count = dbg_count_unreachable(ctx);
	
	if (count == 0)
		sylt_dprintf("no work\n");
	else
		sylt_dprintf(
			"free %ld objs\n", count);
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

string_t* load_file(const char* path, sylt_t* ctx) {
	if (!path) {
		halt(ctx, E_OPEN_FAILED("NULL"));
		unreachable();
	}
	
	FILE* fp = fopen(path, "rb");
	if (!fp) {
		halt(ctx, E_OPEN_FAILED(path));
		unreachable();
	}
	
	/* seek EOF to find the file size */
	fseek(fp, 0, SEEK_END);
	size_t len = ftell(fp);
	fseek(fp, 0, SEEK_SET);
	
	string_t* str = string_new(NULL, len, ctx);
	
	/* read the file contents */
	fread(str->bytes, 1, len, fp);
	fclose(fp);
	
	string_rehash(str, ctx);
	return str;
}

void print_stack_trace(const sylt_t*);
void print_source_line(const func_t*, uint32_t);

/* halts with an error message */
void halt(sylt_t* ctx, const char* fmt, ...) {
	/* print formatted message into buffer */
	char msg[MAX_ERRMSGLEN];
	va_list args;
	va_start(args, fmt);
	vsnprintf(msg, MAX_ERRMSGLEN, fmt, args);
	va_end(args);
		
	/* print file and line if available */
	switch (ctx->state) {
	case SYLT_STATE_COMPILING: {
		comp_t* cmp = ctx->cmp;
		while (cmp->child)
			cmp = cmp->child;
		
		sylt_eprintf("error in ");
		string_eprint(cmp->func->name);
		sylt_eprintf(":%d: %s\n", cmp->prev.line, msg);

		print_source_line(cmp->func, cmp->prev.line);
		break;
	}
	case SYLT_STATE_EXEC: {
		uint32_t line = vm_line(ctx->vm->fp);
		const func_t* func = ctx->vm->fp->func;

		sylt_eprintf("error in ");
		string_eprint(func->name);
		sylt_eprintf(":%d: %s\n", line, msg);

		print_source_line(func, line);
		print_stack_trace(ctx);
		break;
	}
	default: sylt_eprintf("error: %s\n", msg);
	}
	
	fflush(stdout);
	fflush(stderr);

	longjmp(err_jump, 1);
}

void print_stack_trace(const sylt_t* ctx) {
	if (ctx->vm->nframes <= 1)
		return;

	sylt_eprintf("\n[stack trace]:\n");

	for (int64_t i = ctx->vm->nframes - 1; i >= 0; i--) {
		const cframe_t* frame = &ctx->vm->frames[i];
		sylt_eprintf("  %lld. ", i);

		if (i == (int64_t)ctx->vm->nframes - 1)
			sylt_eprintf(">");

		string_eprint(frame->func->path);
		sylt_eprintf(":%d", vm_line(frame));

		if (i == (int64_t)ctx->vm->nframes - 1)
			sylt_eprintf("<");

		sylt_eprintf("\n");
	}
}

void find_source_line(const func_t* func, uint32_t line, size_t* start, size_t* len) {
	assert(start && len && line > 0);

	size_t n = 0;
	for (size_t i = *start; i < func->src->len; i++) {
		uint8_t byte = func->src->bytes[i];

		if (byte == '\n' || byte == '\0' || i == func->src->len - 1) {
			*len = i - *start;

			n++;
			if (n == line)
				break;

			*start = i + 1;
		}
	}
}

/* prints the actual line from the source code */
void print_source_line(const func_t* func, uint32_t line) {
	sylt_eprintf("src: ");
	string_eprint(func->path);
	sylt_eprintf("\n");

	size_t offset = 0;
	size_t len = 0;
	find_source_line(func, line, &offset, &len);
	sylt_eprintf("%5d | %.*s\n", line, (int)len, func->src->bytes + offset);
}

void sylt_handle_halt(sylt_t** ctx) {
	gc_resume(*ctx);
	sylt_free(*ctx);
	*ctx = sylt_new();
}

sylt_t* sylt_new(void) {
	if (setjmp(err_jump))
		return NULL;
	
	sylt_t* ctx = NULL;
	ptr_alloc(ctx, sylt_t, NULL);
	
	set_state(ctx, SYLT_STATE_ALLOC);
	ptr_alloc(ctx->vm, vm_t, ctx);
	ptr_alloc(ctx->cmp, comp_t, ctx);
	
	ctx->mem.objs = NULL;
	ctx->mem.bytes += sizeof(sylt_t);
	ctx->mem.highest = ctx->mem.bytes;
	ctx->mem.count = 0;
	ctx->mem.objcount = 0;
	ctx->mem.gc.marked = NULL;
	ctx->mem.gc.nmarked = 0;
	ctx->mem.gc.trigger = GC_INIT_THRESHOLD;
	ctx->mem.gc.cycles = 0;
	ctx->mem.gc.pause_depth = 0;
	
	ctx->disassemble = false;
	
	set_state(ctx, SYLT_STATE_INIT);
	vm_init(ctx->vm, ctx);
	comp_init(ctx->cmp, NULL, NULL, ctx);
	std_init(ctx);
	
	return ctx;
}

void sylt_free(sylt_t* ctx) {
	if (!ctx)
		return;
	
	set_state(ctx, SYLT_STATE_FREEING);
	
	/* free all unreleased objects */
	gc_free_all(ctx);
	
	/* free the VM */
	if (ctx->vm) {
		vm_free(ctx->vm);
		ptr_free(ctx->vm, vm_t, ctx);
		ctx->vm = NULL;
	}
	
	/* free compiler(s) */
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
	
	/* ensure that no memory was leaked */
	assert(ctx->mem.gc.pause_depth == 0);
	ptrdiff_t bytesleft = ctx->mem.bytes - sizeof(sylt_t);
	if (bytesleft)
		sylt_dprintf("%zu bytes leaked\n", bytesleft);
	assert(!bytesleft);
	
	set_state(ctx, SYLT_STATE_FREE);
	ptr_free(ctx, sylt_t, ctx);
}

void compile_and_run(sylt_t* ctx, string_t* src, string_t* name, comp_t* cmp) {
	set_state(ctx, SYLT_STATE_COMPILING);
	comp_load(cmp, src, name);
	
	/* scan initial token for lookahead */
	step(cmp);
	cmp->prev.line = 1; /* hack */
	
	/* parse the entire source */
	while (!check(cmp, T_EOF)) {
		expr(cmp, ANY_PREC, "top-level expression");
		if (cmp->cur.tag != T_EOF)
			emit_nullary(cmp, OP_POP);
	}
	
	emit_nullary(cmp, OP_RET);
	set_state(ctx, SYLT_STATE_COMPILED);
	
	/* load program */
	sylt_pushclosure(ctx, closure_new(ctx, cmp->func));
	sylt_call(ctx, 0);
	
	/* run program */
	vm_exec(ctx->vm);
}

/* executes a sylt program from a
 * string, returning true if successful */
bool sylt_xstring(sylt_t* ctx, const char* src) {
	if (!ctx || !src)
		return false;
		
	if (setjmp(err_jump)) {
		sylt_handle_halt(&ctx);
		return false;
	}
	
	compile_and_run(ctx, string_lit(src, ctx), string_lit("<input>", ctx), ctx->cmp);
	return true;
}

/* executes a sylt program read from a
 * file, returning true if successful */
bool sylt_xfile(
	sylt_t* ctx, const char* path)
{
	if (!ctx)
		return false;
	
	if (setjmp(err_jump)) {
		sylt_handle_halt(&ctx);
		return false;
	}
	
	compile_and_run(ctx, load_file(path, ctx), string_lit(path, ctx), ctx->cmp);
	return true;
}

void run_tests(sylt_t* ctx) {
	sylt_dprintf("Running tests:\n");
	
	/* empty input */
	assert(sylt_xstring(ctx, ""));
	
	/* make sure errors don't
	 * cause any problems */
	test_errors(ctx);
	
	/* run main tests */
	sylt_xfile(ctx, "tests.sylt");
}

void print_usage(void) {
	sylt_printf("usage: sylt [path|flag(s)]\n");
	sylt_printf("available flags:\n");
	sylt_printf("-help  show this\n");
	sylt_printf("-test  run tests\n");
	sylt_printf("-v     print version\n");
	sylt_printf("-d     disassemble\n");
	sylt_printf("-sbox  sandbox STD library\n");
}

void print_version(void) {
	sylt_printf("%s\n", SYLT_VERSION_STR);
}

int main(int argc, char *argv[]) {
	dbg_print_flags();
	sylt_t* ctx = sylt_new();
	
	int path = -1;
	for (int i = 1; i < argc; i++) {
		string_t* arg = string_lit(argv[i], ctx);
		
		if (arg->bytes[0] != '-') {
			path = i;
		
		} else if (string_eq(arg, string_lit("-help", ctx))) {
			print_usage();
			
		} else if (string_eq(arg, string_lit("-test", ctx))) {
			run_tests(ctx);
			
		} else if (string_eq(arg, string_lit("-v", ctx))) {
			print_version();
		
		} else if (string_eq(arg, string_lit("-d", ctx))) {
			ctx->disassemble = true;
			
		} else if (string_eq(arg, string_lit("-sbox", ctx))) {
			std_sandbox(ctx);
			
		} else {
			print_usage();
			sylt_eprintf(
				"unknown flag: %s\n",
				argv[i]);
		}
	}
	
	if (path == -1) {
		print_usage();
		return EXIT_FAILURE;
	}

	sylt_xfile(ctx, argv[path]);
	
	sylt_free(ctx);
	return EXIT_SUCCESS;
}
