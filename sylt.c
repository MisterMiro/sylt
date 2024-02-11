#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* == debug flags == */

#define DBG_PRINT_SOURCE 1
#define DBG_PRINT_TOKENS 0
#define DBG_PRINT_NAMES 0
#define DBG_PRINT_DATA 0
#define DBG_PRINT_OPCODES 1
#define DBG_PRINT_STACK 0
#define DBG_PRINT_ALLOCS 0
#define DBG_CHECK_MEMLEAKS 1
#define DBG_ASSERT 1

/* == optimization flags == */

#define OPTZ_DEDUP_CONSTANTS 1
#define OPTZ_SPECIAL_PUSHOPS 1
#define OPTZ_EMIT_DUP 0
#define OPTZ_PUSHPOP 0

/* == code limits == */

#define MAX_CODE (UINT16_MAX + 1)
#define MAX_DATA (UINT8_MAX + 1)
#define MAX_STACK (UINT8_MAX + 1)
#define MAX_JUMP UINT16_MAX
#define MAX_CFRAMES 64
#define MAX_PARAMS UINT8_MAX
#define MAX_UPVALUES UINT8_MAX

/* == debug macros == */

#if DBG_ASSERT
#define dbgerr(msg) \
	printf("%s in %s:%d", \
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

typedef enum {
	STATE_INIT,
	STATE_COMPILING,
	STATE_COMPILED,
	STATE_RUNNING,
	STATE_FINISHED,
} sylt_state_t;

typedef struct {
	/* linked list of all objects */
	struct obj_s* objs;
	/* total allocation size */
	ptrdiff_t bytes;
} mem_t;

/* sylt API struct */
typedef struct {
	sylt_state_t state;
	/* compiler */
	struct comp_s* cmp;
	/* virtual machine */
	struct vm_s* vm;
	/* memory */
	mem_t mem;
} sylt_t;

/* == error code + message macros == */

#define E_OUTOFMEM \
	1, "out of memory"
#define E_NOINPUT \
	2, "no input"
#define E_CODELIMIT \
	3, "bytecode limit (%d) reached", MAX_CODE
#define E_DATALIMIT \
	4, "data limit (%d) reached", MAX_DATA
#define E_STACKLIMIT \
	5, "stack limit (%d) reached", MAX_STACK
#define E_JUMPLIMIT \
	6, "jump distance too far (>%d)", MAX_JUMP
#define E_IO(msg, path) \
	7, "%s %s", (msg), (path)
#define E_DIVBYZERO \
	8, "division by zero"
#define E_UNEXPECTEDCHAR(c) \
	9, "unknown character '%c'", (c)
#define E_SYNTAX(msg) \
	10, "%s", (msg)
#define E_TYPE(ex, got) \
	11, "expected %s but got %s", \
		TYPENAME[(ex)], TYPENAME[(got)]
#define E_UNDEFINED(name, len) \
	12, "undefined variable '%.*s'", \
		(name), (len)
#define E_ESCAPESEQ(code) \
	13, "unknown escape sequence '\\%c'", \
		(code)
#define E_UNTERMSTRING \
	14, "unterminated string literal"
#define E_TOOMANYPARAMS \
	15, "too many parameters; limit is %d", \
		(MAX_PARAMS)
#define E_TOOMANYARGS \
	16, "too many arguments; limit is %d", \
		(MAX_PARAMS)
#define E_WRONGARGC(need, got) \
	17, "expected %d arguments but got %d", \
		(need), (got)
#define E_TOOMANYUPVALUES \
	18, "too many upvalues; max is %d", \
		(MAX_UPVALUES)
#define E_STACKOVERFLOW \
	19, "stack overflow"

void halt(
	sylt_t* ctx,
	int code,
	const char* fmt, ...);

/* == memory == */

void* ptr_resize(
	void* p,
	size_t os,
	size_t ns,
	const char* func,
	sylt_t* ctx)
{
	/* keep track of heap size.
	 * the null check is for when we
	 * allocate the ctx struct itself */
	if (ctx)
		ctx->mem.bytes += ns - os;
	
	#if DBG_PRINT_ALLOCS
	printf("  %+ld bytes (%s)\n",
		ns - os, func);
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
#define ptr_alloc(t, ctx) \
	ptr_resize(NULL, 0, sizeof(t), \
		__func__, ctx)
#define ptr_free(p, t, ctx) \
	ptr_resize(p, sizeof(t), 0, \
		__func__, ctx)

/* macros for handling dynamic arrays */
#define arr_alloc(t, n, ctx) \
	ptr_resize(NULL, 0, sizeof(t) * n, \
		__func__, ctx)
#define arr_resize(p, t, os, ns, ctx) \
	ptr_resize(p, \
		sizeof(t) * (os), \
		sizeof(t) * (ns), \
		__func__, ctx)
#define arr_free(p, t, os, ctx) \
	ptr_resize(p, sizeof(t) * (os), 0,\
		__func__, ctx)

/* == opcodes == */

typedef enum {
	/* stack */
	OP_PUSH,
	OP_PUSHNIL,
	OP_PUSHTRUE,
	OP_PUSHFALSE,
	OP_PUSHLIST,
	OP_PUSHFUNC,
	OP_POP,
	OP_DUP,
	OP_SWAP,
	OP_LOAD,
	OP_STORE,
	OP_LOADLIST,
	OP_LOADUP,
	OP_MOVEHEAP,
	OP_HIDERET,
	OP_SHOWRET,
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
	[OP_PUSHNIL] = {"pushnil", 0, +1},
	[OP_PUSHTRUE] = {"pushtrue", 0, +1},
	[OP_PUSHFALSE] = {"pushfalse", 0, +1},
	[OP_PUSHLIST] = {"pushlist", 1, +1},
	[OP_PUSHFUNC] = {"pushfunc", 1, +1},
	[OP_POP] = {"pop", 0, -1},
	[OP_DUP] = {"dup", 0, +1},
	[OP_SWAP] = {"swap", 0, 0},
	[OP_LOAD] = {"load", 1, +1},
	[OP_STORE] = {"store", 1, 0},
	[OP_LOADLIST] = {"loadlist", 0, -1},
	[OP_LOADUP] = {"loadup", 1, +1},
	[OP_MOVEHEAP] = {"moveheap", 0, -1},
	[OP_HIDERET] = {"hideret", 0, -1},
	[OP_SHOWRET] = {"showret", 0, +1},
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

typedef struct {
	/* bytecode */
	uint8_t* code;
	size_t ncode;
	/* constant data; stores any
	 * literal values found in
	 * the source code */
	struct value_s* data;
	size_t ndata;
	/* total stack slots needed */
	size_t slots;
	/* name, for errors and debugging */
	struct string_s* name;
	struct string_s* fullname;
	/* list of line numbers */
	int32_t* lines;
	size_t nlines;
} chunk_t;

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

static const char* TYPENAME[] = {
	"Nil",
	"Bool",
	"Num",
	"List",
	"String",
	"Function",
	"Closure",
	"Upvalue"
};

#define isheaptype(t) \
	((t) == TYPE_LIST \
		|| (t) == TYPE_STRING \
		|| (t) == TYPE_FUNCTION \
		|| (t) == TYPE_CLOSURE \
		|| (t) == TYPE_UPVALUE)

/* heap-allocated object header */
typedef struct obj_s {
	type_t tag;
	/* linked list of all objects */
	struct obj_s* next;
} obj_t;

/* tagged enum representing a sylt value */
typedef struct value_s {
	type_t tag;
	union {
		double num;
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
	/* bytecode chunk */
	chunk_t chunk;
	/* if set this calls a C function
	 * and the bytecode chunk is unused */
	cfunc_t cfunc;
	int params;
	int upvalues;
} func_t;

/* closure object;
 * wraps a function and captures its
 * surrounding context, for example a
 * variable declared outside of but
 * referenced inside the function body */
typedef struct {
	obj_t obj;
	const func_t* func;
	struct upvalue_s** upvals;
	size_t nupvals;
} closure_t;

/* upvalue object;
 * used to reference a value outside a
 * functions local stack window */
typedef struct upvalue_s {
	obj_t obj;
	/* stack index of live value on stack
	 * or -1 if closed */
	int pos;
	/* if the value in the above slot goes
	 * off the stack it gets copied here */
	struct value_s closed;
	struct upvalue_s* next;
} upvalue_t;

/* macros for creating a value_t
 * struct from a raw value */
#define newnil() \
	(value_t){TYPE_NIL, {.num = 0}}
#define newbool(v) \
	(value_t){TYPE_BOOL, {.num = (v)}}
#define newnum(v) \
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

void chunk_init(chunk_t*, string_t*);
void chunk_free(chunk_t*, sylt_t*);

obj_t* obj_new(
	size_t size,
	type_t tag,
	sylt_t* ctx)
{
	assert(isheaptype(tag));
	
	obj_t* obj = ptr_resize(
		NULL, 0, size, __func__, ctx);
	obj->tag = tag;
	obj->next = ctx->mem.objs;
	ctx->mem.objs = obj;
	return obj;
}

void obj_free(obj_t* obj, sylt_t* ctx) {
	switch (obj->tag) {
	case TYPE_LIST: {
		list_t* list = (list_t*)obj;
		arr_free(
			list->items,
			value_t,
			list->len,
			ctx);
		ptr_free(list, list_t, ctx);
		break;
	}
	case TYPE_STRING: {
		string_t* str = (string_t*)obj;
		arr_free(
			str->bytes,
			uint8_t,
			str->len,
			ctx);
		
		ptr_free(str, string_t, ctx);
		break;
	}
	case TYPE_FUNCTION: {
		func_t* func = (func_t*)obj;
		chunk_free(&func->chunk, ctx);
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

/* creates an empty list */
list_t* list_new(sylt_t* ctx) {
	list_t* ls = (list_t*)obj_new(
		sizeof(list_t), TYPE_LIST, ctx);
	ls->items = NULL;
	ls->len = 0;
	return ls;
}

/* returns the item at the given index */
value_t list_get(list_t* ls, int index) {
	if (index < 0) {
		int mod = ls->len + index;
		if (mod < 0)
			return newnil();
		return ls->items[mod];
	}
	
	if (index > ls->len - 1)
		return newnil();
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
		return newnil();
	value_t last = list_get(ls, -1);
	ls->items = arr_resize(
		ls->items,
		value_t,
		ls->len,
		ls->len - 1,
		ctx);
	ls->len--;
	return last;
}

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
		str->bytes = (uint8_t*)bytes;
		
	} else {
		str->bytes =
			arr_alloc(uint8_t, len, ctx);
		memcpy(str->bytes, bytes, len);
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
		
	char* bytes =
		arr_alloc(char, len, ctx);
	vsnprintf(
		bytes, len + 1, fmt, args);
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
	uint8_t* bytes =
		arr_alloc(uint8_t, len, ctx);
		
	memcpy(bytes, a->bytes, a->len);
	memcpy(bytes + a->len, b->bytes, b->len);
	
	return string_new(
		(const char*)bytes,
		len, true, ctx);
}

/* appends src to dst */
void string_append(
	string_t** dst,
	const string_t* src,
	sylt_t* ctx)
{
	*dst = string_concat(*dst, src, ctx);	
}

/* appends literal src to dst */
void string_append_lit(
	string_t** dst,
	const char* src,
	sylt_t* ctx)
{
	*dst = string_concat(
		*dst, string_lit(src, ctx), ctx);	
}

/* %s can not be used since sylt strings are
 * not required to be zero-terminated */
void string_print(string_t* str) {
	printf("%.*s", (int)str->len, str->bytes);
}

/* creates a new function */
func_t* func_new(
	sylt_t* ctx, string_t* name)
{
	func_t* func = (func_t*)obj_new(
		sizeof(func_t), TYPE_FUNCTION, ctx);
	chunk_init(&func->chunk, name);
	func->cfunc = NULL;
	func->params = 0;
	func->upvalues = 0;
	return func;
}

/* creates a new C function */
func_t* func_newc(
	sylt_t* ctx,
	string_t* name,
	cfunc_t cfunc,
	int params)
{
	func_t* func = func_new(ctx, name);
	func->cfunc = cfunc;
	func->params = params;
	return func;
}

/* creates a new closure around a function */
closure_t* closure_new(
	sylt_t* ctx, const func_t* func)
{
	upvalue_t** upvals = arr_alloc(
		upvalue_t*, func->upvalues, ctx);
	for (size_t i = 0;
		i < func->upvalues; i++)
		upvals[i] = NULL;
		
	closure_t* cls = (closure_t*)obj_new(
		sizeof(closure_t), TYPE_CLOSURE, ctx);
	cls->func = func;
	cls->upvals = upvals;
	cls->nupvals = func->upvalues;
	return cls;
}

/* creates a new upvalue */
upvalue_t* upvalue_new(
	sylt_t* ctx, size_t pos)
{
	upvalue_t* upval = (upvalue_t*)obj_new(
		sizeof(upvalue_t), TYPE_UPVALUE, ctx);
	upval->pos = pos;
	upval->closed = newnil();
	upval->next = NULL;
	return upval;
}

/* returns true if the values are equal */
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
	case TYPE_LIST:
		return getobj(a) == getobj(b);
	case TYPE_STRING: {
		string_t* astr = getstring(a);
		string_t* bstr = getstring(b);
		return astr->len == bstr->len
			&& !memcmp(
				astr->bytes,
				bstr->bytes,
				astr->len);
	}
	case TYPE_FUNCTION:
	case TYPE_CLOSURE:
	case TYPE_UPVALUE: {
		return getobj(a) == getobj(b);
	}
	default: unreachable();
	}
}

/* converts a value to a printable string */
string_t* val_tostring(
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
		size_t len = snprintf(
			NULL, 0, "%g", getnum(val));
		
		char* bytes =
			arr_alloc(char, len, ctx);
		snprintf(
			bytes,
			len + 1, "%g", getnum(val));
		
		return string_new(
			bytes, len, true, ctx);
	}
	case TYPE_LIST: {
		string_t* str = string_lit("[", ctx);
		
		/* print items */
		list_t* ls = getlist(val);
		for (size_t i = 0; i < ls->len; i++) {
			string_t* vstr = val_tostring(
				ls->items[i], ctx);
			string_append(&str, vstr, ctx);
			
			/* add ', ' between items */
			if (i < ls->len - 1) {
				string_t* sep =
					string_lit(", ", ctx);
				string_append(&str, sep, ctx);
			}
		}
		
		return string_concat(
			str, string_lit("]", ctx), ctx);
	}
	case TYPE_STRING: {
		return getstring(val);
	}
	case TYPE_FUNCTION: {
		return getfunc(val)->chunk.name;
	}
	case TYPE_CLOSURE: {
		return getclosure(val)->
			func->chunk.name;
	}
	case TYPE_UPVALUE: {
		return string_lit("upval", ctx);
	}
	default: unreachable();
	}
	
	return NULL;
}

string_t* val_tostring_opts(
	value_t val,
	bool quotestr,
	int maxlen,
	sylt_t* ctx)
{
	string_t* str = string_empty(ctx);
	string_t* vstr = val_tostring(val, ctx);
	
	if (quotestr && val.tag == TYPE_STRING) {
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
				"'%.*s'", len, vstr->bytes),
			ctx);
		
		if (shortened)
			string_append(
				&str,
				string_fmt(
					ctx,
					"(..+%ld)",
					str->len - maxlen),
				ctx);
	} else {
		str = vstr;
		
		if (val.tag == TYPE_CLOSURE)
			string_append(
				&str,
				string_lit("()", ctx),
				ctx);
	}
	
	return str;
}

/* prints a value to stdout */
void val_print(
	value_t val,
	bool quotestr,
	int maxlen,
	sylt_t* ctx)
{
	string_print(val_tostring_opts(
		val, quotestr, maxlen, ctx));
}

void chunk_init(
	chunk_t* chunk, string_t* name)
{
	chunk->code = NULL;
	chunk->ncode = 0;
	chunk->data = NULL;
	chunk->ndata = 0;
	chunk->slots = 0;
	chunk->name = name;
	chunk->fullname = name;
	chunk->lines = NULL;
	chunk->nlines = 0;
}

void chunk_free(
	chunk_t* chunk,
	sylt_t* ctx)
{
	arr_free(
		chunk->code, uint8_t, chunk->ncode,
		ctx);
    arr_free(
    	chunk->data, value_t, chunk->ndata,
    	ctx);
    arr_free(
    	chunk->lines, int32_t, chunk->nlines,
    	ctx);
    chunk_init(chunk, NULL);
}

/* writes a byte to the code array */
void chunk_write(
	chunk_t* chunk,
	uint8_t byte,
	int32_t line,
	sylt_t* ctx)
{
	if (chunk->ncode >= MAX_CODE) {
		halt(ctx, E_CODELIMIT);
		return;
	}
	
	chunk->code = arr_resize(
		chunk->code,
		uint8_t,
		chunk->ncode,
		chunk->ncode + 1,
		ctx);
	chunk->code[chunk->ncode++] = byte;
	
	/* map line number to byte */
	chunk->lines = arr_resize(
		chunk->lines,
		uint32_t,
		chunk->nlines,
		chunk->nlines + 1,
		ctx);
	chunk->lines[chunk->nlines++] = line;
}

/* writes a value to the constant data table
 * and returns the index it was written to */
size_t chunk_write_data(
	chunk_t* chunk, value_t val, sylt_t* ctx)
{
	#if OPTZ_DEDUP_CONSTANTS
	/* check if a constant with the same
	 * value already exists and if so
	 * return its index */
	for (
		size_t i = 0; i < chunk->ndata; i++)
	{
		if (val_eq(chunk->data[i], val))
			return i;
	}
	#endif
	
	if (chunk->ndata >= MAX_DATA) {
		halt(ctx, E_DATALIMIT);
		unreachable();
	}
		
	chunk->data = arr_resize(
		chunk->data,
		value_t,
		chunk->ndata,
		chunk->ndata + 1,
		ctx);
	
	chunk->data[chunk->ndata++] = val;
	return chunk->ndata - 1;
}

/* == virtual machine == */

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

/* stores execution state */
typedef struct vm_s {
	/* call stack */
	cframe_t frames[MAX_CFRAMES];
	size_t nframes;
	cframe_t* fp;
	/* value stack */
	value_t* stack;
	size_t maxstack;
	/* points to top of stack + 1 */
	value_t* sp;
	/* linked list of open upvalues */
	upvalue_t* openups;
	/* special slot for return value */
	value_t hidden;
	/* reference to API */
	sylt_t* ctx;
} vm_t;

void vm_init(vm_t* vm, sylt_t* ctx) {
	vm->nframes = 0;
	vm->fp = NULL;
	vm->stack = NULL;
	vm->maxstack = 0;
	vm->sp = NULL;
	vm->openups = NULL;
	vm->hidden = newnil();
	vm->ctx = ctx;
}

void vm_free(vm_t* vm) {
	arr_free(
		vm->stack, 
		value_t,
		vm->maxstack,
		vm->ctx);
	vm_init(vm, NULL);
}

void dbg_print_header(
	const vm_t* vm, const closure_t* cls)
{
	const func_t* func = cls->func;
	const chunk_t* chunk = &func->chunk;
	
	printf("\n-> ");
	string_print(chunk->fullname);
	printf(" \n");
	
	printf("depth %d/%d, ",
		(int)vm->nframes, MAX_CFRAMES);
	
	size_t used = vm->sp - vm->stack;
	printf("stack %ld/%ld\n",
		used, vm->maxstack);
	
	printf("%ld bytes, ", chunk->ncode);
	printf("%ld constants, ", chunk->ndata);
	printf("%ld stack slots, ", chunk->slots);
	
	printf("%d params, ", func->params);
	printf("%ld upvalues\n", cls->nupvals);
	
	#if DBG_PRINT_DATA
	puts("  constants:");
	for (size_t i = 0; i < chunk->ndata; i++)
	{
		printf("    #%-2ld -- ", i);
		val_print(chunk->data[i],
			true, 24, vm->ctx);
		putchar('\n');
	}
	#endif
		
	puts("  addr  line opcode           hex");
	printf("  ");
	for (int i = 0; i < 40; i++)
		putchar('-');
	putchar('\n');
}

void dbg_print_instruction(
	const vm_t* vm, const chunk_t* chunk)
{
	op_t op = vm->fp->ip[-1];
	size_t addr =
		(size_t)
			(vm->fp->ip - chunk->code - 1);
	printf("  %05ld", addr);
	
	const int32_t* lines =
		vm->fp->func->chunk.lines;
	int32_t line = lines[addr];
	
	if (line > 0 && line != lines[addr - 1])
		printf(" %-4d ", line);
	else
		printf(" |    ");
		
	const char* name = OPINFO[op].name;
	printf("%-17s", name);
		
	/* hex values */
	int rank = OPINFO[op].rank;
	for (int i = -1; i < rank; i++) {
		uint8_t arg = vm->fp->ip[i];
		printf("%02d ", arg);
	}
		
	printf("\n");
}

void dbg_print_stack(
	const vm_t* vm, const chunk_t* chunk)
{
	const int maxvals = 5;
	
	/* don't print first iteration */
	if (vm->fp->ip - 1 == chunk->code)
		return;
	
	value_t* start =
		vm->stack + vm->fp->offs;
	
	/* only display the last N values */
	ptrdiff_t diff = vm->sp - start;
	if (diff > maxvals)
		start += diff - maxvals;
	
	value_t* v = start;
	
	if (diff > maxvals)
		printf("  [ <+%ld, ", diff - maxvals);
	else
		printf("  [ ");
	
	for (; v != vm->sp; v++) {
		val_print(*v, true, 12, vm->ctx);
		if (v != vm->sp - 1)
		  printf(", ");
	}
	
	printf(" ]\n");
}

/* pushes a value on the stack */
inline static void sylt_push(
	vm_t* vm, value_t val)
{
	#if DBG_ASSERT
	size_t used = vm->sp - vm->stack;
	assert(used <= vm->maxstack);
	#endif
	
	*vm->sp++ = val;
}

/* pops a value from the stack */
inline static value_t sylt_pop(vm_t* vm) {
	return *(--vm->sp);
}

/* returns the value n down the stack */
inline static value_t sylt_peek(
	vm_t* vm, int n
) {
	//assert(depth > 0);
	return vm->sp[-1 - n];
}

/* virtual machine macros */
#define read() *vm->fp->ip++
#define read16() \
	(vm->fp->ip += 2, (uint16_t) \
		((vm->fp->ip[-2] << 8) \
			| vm->fp->ip[-1]))
#define push(v) sylt_push(vm, (v))
#define pop() sylt_pop(vm)
#define peek(n) sylt_peek(vm, (n))

void vm_math(vm_t* vm, op_t opcode);

/* grows the stack if necessary */
void vm_growstack(vm_t* vm, int needed) {
	if (vm->maxstack >= needed)
		return;
		
	size_t offset = vm->sp - vm->stack;
	
	vm->stack = arr_resize(
		vm->stack,
		value_t,
		vm->maxstack,
		needed,
		vm->ctx);	
	vm->maxstack = needed;
	
	vm->sp = vm->stack + offset;
}

value_t vm_getupval(
	vm_t* vm, upvalue_t* upval)
{
	if (upval->pos == -1)
		return upval->closed;
	return vm->stack[upval->pos];
}

upvalue_t* vm_capupval(
	vm_t* vm, size_t pos)
{
	upvalue_t* prev = NULL;
	upvalue_t* cur = vm->openups;
	while (cur && cur->pos > pos) {
		prev = cur;
		cur = cur->next;
	}
	
	if (cur && cur->pos == pos)
		return cur;
	
	upvalue_t* fresh = upvalue_new(
		vm->ctx, pos);
	
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
	if (vm->openups
		&& vm->openups->pos >= last)
	{
		upvalue_t* upval = vm->openups;
		upval->closed = vm->stack[upval->pos];
		upval->pos = -1;
		vm->openups = upval->next;
	}
}

void vm_exec(vm_t* vm, const func_t* entry) {
	vm->ctx->state = STATE_RUNNING;
	
	const int INIT_STACK = 0;
	int init_stack_size = entry->chunk.slots;
	if (init_stack_size < INIT_STACK)
		init_stack_size = INIT_STACK;
	
	/* init stack */
	vm_growstack(vm, init_stack_size);
	
	/* init stack pointer */
	vm->sp = vm->stack;
	
	/* setup first call frame */
	cframe_t frame;
	frame.func = entry;
	frame.cls = closure_new(vm->ctx, entry);
	frame.ip = entry->chunk.code;
	frame.offs = 0;
	vm->frames[vm->nframes++] = frame;
	vm->fp = &vm->frames[vm->nframes - 1];
	
	#if DBG_PRINT_OPCODES
	dbg_print_header(vm, vm->fp->cls);
	#endif
	
	for (;;) {
		uint8_t op = read();
		
		#if DBG_PRINT_STACK
		dbg_print_stack(vm,
			&vm->fp->func->chunk);
		#endif
		
		#if DBG_PRINT_OPCODES
		dbg_print_instruction(vm,
			&vm->fp->func->chunk);
		#endif
		
		switch (op) {
		/* stack */
		case OP_PUSH: {
			value_t val = vm->fp->func->
				chunk.data[read()];
			push(val);
			break;
		}
		case OP_PUSHNIL: {
			push(newnil());
			break;
		}
		case OP_PUSHTRUE: {
			push(newbool(true));
			break;
		}
		case OP_PUSHFALSE: {
			push(newbool(false));
			break;
		}
		case OP_PUSHLIST: {
			uint8_t len = read();
			list_t* ls = list_new(vm->ctx);
			
			push(wraplist(ls)); /* for GC */
			for (int i = len; i > 0; i--) {
				list_push(
					ls, peek(i), vm->ctx);
			}
			pop();
			
			vm->sp -= len;
			
			push(wraplist(ls));
			break;
		}
		case OP_PUSHFUNC: {
			func_t* func = 
				getfunc(vm->fp->func->
					chunk.data[read()]);
			
			closure_t* cls =
				closure_new(vm->ctx, func);
			push(wrapclosure(cls));
			
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
		case OP_LOADLIST: {
			value_t index = pop();
			typecheck(
				vm->ctx, index, TYPE_NUM);
				
			value_t list = pop();
			typecheck(
				vm->ctx, list, TYPE_LIST);
			
			push(list_get(
				getlist(list),
				getnum(index)));
			break;
		}
		case OP_LOADUP: {
			uint8_t index = read();
			value_t val = vm_getupval(vm,
				vm->fp->cls->upvals[index]);
			push(val);
			break;
		}
		case OP_MOVEHEAP: {
			size_t pos = 
				vm->sp - vm->stack - 1;
			vm_closeupvals(vm, pos);
			pop();
			break;
		}
		case OP_HIDERET: {
			vm->hidden = pop();
			break;
		}
		case OP_SHOWRET: {
			push(vm->hidden);
			break;
		}
		/* arithmetic */
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
			push(newnum(-getnum(pop())));
			break;
		}
		/* comparison */
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
			push(newbool(val_eq(a, b)));
			break;
		}
		case OP_NEQ: {
			value_t b = pop();
			value_t a = pop();
			push(newbool(!val_eq(a, b)));
			break;
		}
		case OP_NOT: {
			typecheck(
				vm->ctx,
				vm->sp[-1],
				TYPE_BOOL);
			push(newbool(!getbool(pop())));
			break;
		}
		/* control flow */
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
				vm->sp -= argc + 1;
				push(result);
				
				#if DBG_PRINT_OPCODES
				printf("\n");
				#endif
				
				break;
			}
			
			if (vm->nframes == MAX_CFRAMES) {
				halt(vm->ctx,
					E_STACKOVERFLOW);
				unreachable();
			}
			
			/* see if we have enough 
			 * stack space and grow if not */
			size_t used = vm->sp - vm->stack;
			size_t needed = used
				+ func->chunk.slots;
			vm_growstack(vm, needed);
			
			/* push a new call frame */
			cframe_t frame;
			frame.func = func;
			frame.cls = cls;
			frame.ip = func->chunk.code;
			frame.offs = used - argc;
			vm->frames[vm->nframes++] = frame;
			vm->fp = &vm->frames[
				vm->nframes - 1];
				
			#if DBG_PRINT_OPCODES
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
				vm->ctx->state
					= STATE_FINISHED;
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
			
			#if DBG_PRINT_OPCODES
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
	double b = getnum(pop());
	
	typecheck(vm->ctx, peek(0), TYPE_NUM);
	double a = getnum(pop());
	
	switch (opcode) {
	/* arithmetic */
	case OP_ADD: push(newnum(a + b)); break;
	case OP_SUB: push(newnum(a - b)); break;
	case OP_MUL: push(newnum(a * b)); break;
	case OP_DIV:
		if (b == 0.0f) {
			halt(vm->ctx, E_DIVBYZERO);
			unreachable();
		}
		push(newnum(a / b));
		break;
	case OP_EDIV: {
		if (b == 0.0f) {
			halt(vm->ctx, E_DIVBYZERO);
			unreachable();
		}
		double res = fmod(a, b);
		if (res < 0)
			res += fabs(b);
		push(newnum(res));
		break;
	}
	/* comparison */
	case OP_LT: push(newbool(a < b)); break;
	case OP_LTE: push(newbool(a <= b)); break;
	case OP_GT: push(newbool(a > b)); break;
	case OP_GTE: push(newbool(a >= b)); break;
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
#define arg(n) (*(ctx->vm->sp - argc()))

/* == prelude == */

value_t slib_put(sylt_t* ctx) {
	for (int i = 0; i < argc(); i++)
		val_print(arg(0), false, 0, ctx);
	return newnil();
}

value_t slib_ensure(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_BOOL);
	if (!getbool(arg(0))) {
		halt(ctx, -1, "ensure failed");
		unreachable();
	}
	return arg(0);
}

value_t slib_length(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_LIST);
	return newnum(getlist(arg(0))->len);
}

value_t slib_push(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_LIST);
	list_push(getlist(arg(0)), arg(1), ctx);
	return newnil();
}

value_t slib_pop(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_LIST);
	return list_pop(getlist(arg(0)), ctx);
}

/* == math == */

value_t slib_abs(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return newnum(fabs(getnum(arg(0))));
}

value_t slib_raise(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	typecheck(ctx, arg(1), TYPE_NUM);
	return newnum(powf(
			getnum(arg(0)),
			getnum(arg(1))));
}

value_t slib_sqrt(sylt_t* ctx) {
	typecheck(ctx, arg(0), TYPE_NUM);
	return newnum(sqrt(getnum(arg(0))));
}

void slib_add(
	sylt_t* ctx,
	const char* name,
	cfunc_t cfunc,
	int params);

void load_slib(sylt_t* ctx) {
	/* prelude */
	slib_add(ctx, "put", slib_put, 1);
	slib_add(ctx, "ensure", slib_ensure, 1);
	slib_add(ctx, "length", slib_length, 1);
	slib_add(ctx, "push", slib_push, 2);
	slib_add(ctx, "pop", slib_pop, 1);
	
	/* math */
	slib_add(ctx, "abs", slib_abs, 1);
	slib_add(ctx, "raise", slib_raise, 2);
	slib_add(ctx, "sqrt", slib_sqrt, 1);
}

/* == compiler == */

typedef enum {
	T_NAME,
	T_NIL,
	T_TRUE,
	T_FALSE,
	T_LET,
	T_IF,
	T_ELSE,
	T_AND,
	T_OR,
	T_STRING,
	T_NUMBER,
	T_PLUS,
	T_MINUS,
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
} token_t;

/* operator precedence levels */
typedef enum {
	PREC_NONE,
	PREC_ASSIGN,
	PREC_OR,
	PREC_AND,
	PREC_EQ,
	PREC_CMP,
	PREC_TERM,
	PREC_FACTOR,
	PREC_UNARY,
	PREC_CALL,
	PREC_INDEX,
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
	/* output */
	func_t* func;
	/* current (simulated) stack size */
	int curslots;
	
	/* last opcode written to chunk */
	int lastop;
	/* operand of last OP_LOAD */
	int lastload;
	/* last value pushed to stack */
	value_t lastpush;
	
	/* source code */
	string_t* src;
	/* scanner position */
	char* pos;
	/* scanner line */
	int32_t line;
	/* parsing tokens */
	token_t prev;
	token_t cur;
	/* symbol table */
	symbol_t* names;
	size_t nnames;
	/* current scope depth */
	int depth;
	/* upvalues */
	cmp_upvalue_t upvals[MAX_UPVALUES];
	/* reference to API */
	sylt_t* ctx;
} comp_t;

void comp_init(
	comp_t* cmp,
	string_t* src,
	string_t* src_name,
	sylt_t* ctx)
{
	cmp->parent = NULL;
	cmp->func = func_new(ctx, src_name);
	cmp->curslots = 0;
	
	cmp->lastop = -1;
	cmp->lastload = -1;
	cmp->lastpush =
		(value_t){-1, {.num = 0}};
	
	cmp->src = src;
	cmp->pos = (char*)cmp->src->bytes;
	cmp->line = 1;
	cmp->names = NULL;
	cmp->nnames = 0;
	cmp->depth = 0;
	cmp->ctx = ctx;
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
	#define token(t) \
		(token_t){t, start, cmp->pos - start}
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
		if (keyword("if"))
			return token(T_IF);
		if (keyword("else"))
			return token(T_ELSE);
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
	case '-': return token(T_MINUS);
	case '*': return token(T_STAR);
	case '/': return token(T_SLASH);
	case '%': return token(T_PERCENT);
	case '<':
		return token((!match('=')) ?
			T_LESS :
			T_LESS_EQ);
	case '>':
		return token((!match('=')) ?
			T_GREATER : 
			T_GREATER_EQ);
	case '=':
		return token((!match('=')) ?
			T_EQ :
			T_EQ_EQ);
	case '!':
		return token((!match('=')) ?
			T_BANG :
			T_BANG_EQ);
	case '(': return token(T_LPAREN);
	case ')': return token(T_RPAREN);
	case '{': return token(T_LCURLY);
	case '}': return token(T_RCURLY);
	case '[': return token(T_LSQUARE);
	case ']': return token(T_RSQUARE);
	case ',': return token(T_COMMA);
	}
	
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

/* helper function to get the output chunk */
chunk_t* comp_chunk(comp_t* cmp) {
	return &cmp->func->chunk;
}

void comp_simstack(comp_t* cmp, int n) {
	cmp->curslots += n;
	assert(cmp->curslots >= 0);
	
	if (cmp->curslots > MAX_STACK) {
		halt(cmp->ctx, E_STACKLIMIT);
		unreachable();
	}
	
	/* record the largest stack size */
	chunk_t* chunk = comp_chunk(cmp);
	if (cmp->curslots > chunk->slots)
		chunk->slots = cmp->curslots;
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
	
	#if OPTZ_PUSHPOP
	/* prevents emitting code when pushing
	 * a value and immediately popping it */
	bool waspush =
		cmp->lastop == OP_PUSH;
		&& cmp->lastop == OP_CLOSE;
	
	if (op == OP_POP && waspush) {
		int shrink =
			1 + OPINFO[cmp->lastop].rank;
		
		/* delete the last opcode
		 * plus its operand(s) */
		chunk->code = arr_resize(
			chunk->code,
			uint8_t,
			chunk->ncode,
			chunk->ncode - shrink,
			cmp->ctx);
		chunk->ncode -= shrink;
		
		/* delete the value from the value
		 * pool if one was written */
		if (true) {
			chunk->pool = arr_resize(
				chunk->pool,
				value_t,
				chunk->npool,
				chunk->npool - 1,
				cmp->ctx);
			chunk->npool--;
		}
		
		/* shrink simulated stack size */
		comp_simstack(cmp,
			-OPINFO[cmp->lastop].effect);
		
		cmp->lastop = -1;
		return;
	}
	#endif
	
	/* write the instruction opcode */
	chunk_write(
		comp_chunk(cmp),
		op,
		cmp->line,
		cmp->ctx);
	cmp->lastop = op;
	
	/* write the arguments */
	for (size_t i = 0; i < nargs; i++) {
		chunk_write(
			comp_chunk(cmp),
			args[i],
			cmp->line,
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
	#if OPTZ_EMIT_DUP
	if (cmp->lastpush.tag != -1
		&& val_eq(cmp->lastpush, val))
	{
		emit_nullary(cmp, OP_DUP);
		return;
	}
	cmp->lastpush = val;
	#endif
	
	#if OPTZ_SPECIAL_PUSHOPS
	switch (val.tag) {
	case TYPE_NIL: {
		emit_nullary(cmp, OP_PUSHNIL);
		return;
	}
	case TYPE_BOOL: {
		emit_nullary(cmp, (getbool(val))
			? OP_PUSHTRUE : OP_PUSHFALSE);
		return;
	}
	default: break;
	}
	#endif
	
	size_t slot = chunk_write_data(
		comp_chunk(cmp), val, cmp->ctx);
		
	if (val.tag == TYPE_FUNCTION) {
		emit_unary(cmp, OP_PUSHFUNC, slot);
	} else {
		emit_unary(cmp, OP_PUSH, slot);
	}
}

/* emits a jump instruction followed by
 * two placeholder bytes, returning an
 * address used for backpatching them
 * into a 16-bit jump offset later on */
int emit_jump(comp_t* cmp, op_t opcode) {
	emit_binary(cmp, opcode, 0xff, 0xff);
	return comp_chunk(cmp)->ncode - 2;
}

/* takes the return value of emit_jump
 * after we've emitted the code we need to
 * jump over and backpatches the jump target
 * short to the correct offset */
void patch_jump(comp_t* cmp, int addr) {
	int dist =
		comp_chunk(cmp)->ncode - addr - 2;
	if (dist > MAX_JUMP) {
		halt(cmp->ctx, E_JUMPLIMIT);
		return;
	}
	
	/* encode short as two bytes */
	comp_chunk(cmp)->code[addr] =
		(dist >> 8) & 0xff;
	comp_chunk(cmp)->code[addr + 1] =
		dist & 0xff;
}

/* adds a name to the symbol table and
 * returns the index */
int add_symbol(comp_t* cmp, token_t name) {
	#if DBG_PRINT_NAMES
	for (int i = 0; i < cmp->depth; i++)
		printf("  ");
	printf("(sym) %.*s\n",
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
		//printf("%.*s\n",
		//	(int)other.len, other.start);
		
		bool equal = name.len == other.len
			&& memcmp(
				name.start,
				other.start,
				name.len
			) == 0;
		if (equal)
			return i;
	}
	
	return -1;
}

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
	printf("%-4d '%.*s'\n",
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
	
	halt(cmp->ctx, E_SYNTAX(msg));
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
void if_else(comp_t*);
void block(comp_t*);

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
	[T_NIL] = {literal, NULL, PREC_NONE},
	[T_TRUE] = {literal, NULL, PREC_NONE},
	[T_FALSE] = {literal, NULL, PREC_NONE},
	[T_LET] = {let, NULL, PREC_NONE},
	[T_IF] = {if_else, NULL, PREC_NONE},
	[T_ELSE] = {NULL, NULL, PREC_NONE},
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
	[T_LPAREN] = {grouping, call, PREC_CALL},
	[T_RPAREN] = {NULL, NULL, PREC_NONE},
	[T_LCURLY] = {block, NULL, PREC_NONE},
	[T_RCURLY] = {NULL, NULL, PREC_NONE},
	[T_LSQUARE] = {list, index, PREC_INDEX},
	[T_RSQUARE] = {NULL, NULL, PREC_NONE},
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
		halt(cmp->ctx, -1,
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
	switch (cmp->prev.tag) {
	case T_NIL:
		emit_value(cmp, newnil());
		break;
	case T_TRUE:
		emit_value(cmp, newbool(true));
		break;
	case T_FALSE:
		emit_value(cmp, newbool(false));
		break;
	default: unreachable();
	}
}

/* parses a symbol name */
void name(comp_t* cmp) {
	int index = find_symbol(cmp, cmp->prev);
	if (index != -1) {
		#if OPTZ_EMIT_DUP
		if (index == cmp->lastload) {
			emit_nullary(cmp, OP_DUP);
			return;
		}
		cmp->lastload = index;
		#endif
		
		emit_unary(cmp, OP_LOAD, index);
		return;
	}
	
	index = find_upvalue(cmp, cmp->prev);
	if (index != -1) {
		emit_unary(cmp, OP_LOADUP, index);
		return;
	}
	
	halt(cmp->ctx, E_UNDEFINED(
		cmp->prev.len, cmp->prev.start));
}

/* parses a list literal */
void list(comp_t* cmp) {
	int len = 0;
	while (!check(cmp, T_RSQUARE)
		&& !check(cmp, T_EOF))
	{
		expr(cmp, PREC_OR);
		len++;
		
		if (!match(cmp, T_COMMA))
			break;
	}
	
	eat(cmp, T_RSQUARE, "expected ']'");
	emit_unary(cmp, OP_PUSHLIST, len);
}

/* parses a string literal */
void string(comp_t* cmp) {
	token_t token = cmp->prev;
	assert(token.len >= 2); /* "" */
	
	/* allocate a buffer the same size
	 * as the length of the string literal */
	char* str = arr_alloc(
		char,
		token.len - 2,
		cmp->ctx);
	size_t strlen = 0;
	
	/* expand escape sequences */
	size_t end = token.len - 1;
	for (size_t i = 1; i < end;) {
		char c = token.start[i];
		if (c == '\\' && i <= end - 1) {
			char next = token.start[i + 1];
			
			switch (next) {
			case '\\':
				str[strlen++] = '\\';
				break;
			case '\"':
				str[strlen++] = '\"';
				break;
			case 'r':
				str[strlen++] = '\r';
				break;
			case 'n':
				str[strlen++] = '\n';
				break;
			case 't':
				str[strlen++] = '\t';
				break;
			case '0':
				str[strlen++] = '\0';
				break;
			default:
				halt(cmp->ctx,
					E_ESCAPESEQ(next));
			}
			
			i += 2;
		} else {
			str[strlen++] = c;
			i++;
		}
	}
	
	/* shrink to fit */
	if (strlen < token.len - 2) {
		str = arr_resize(str,
			char,
			token.len - 2,
			strlen,
			cmp->ctx);
	}
	
	/* the string object takes ownership
	 * of the buffer so we don't need
	 * to free it */
	string_t* strobj = string_new(
		str, strlen, true, cmp->ctx);
	emit_value(cmp, wrapstring(strobj));
}

/* parses a numeric literal */
void number(comp_t* cmp) {
	double num =
		strtod(cmp->prev.start, NULL);
	emit_value(cmp, newnum(num));
}

/* parses a parenthesized expression */
void grouping(comp_t* cmp) {
	expr(cmp, ANY_PREC);
	eat(cmp, T_RPAREN,
		"expected closing ')'");
}

/* parses a unary expression */
void unary(comp_t* cmp) {
	/* save the operator token */
	token_type_t token = cmp->prev.tag;
	
	/* parse the operand */
	expr(cmp, PREC_UNARY);
	
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
	expr(cmp, PREC_OR);
	eat(cmp, T_RSQUARE, "expected ']'");
	emit_nullary(cmp, OP_LOADLIST);
}

void parse_func(comp_t* cmp, token_t name);

/* parses a variable declaration */
void let(comp_t* cmp) {
	eat(cmp, T_NAME,
		"expected variable name after 'let'");
	
	/* remember the name token */
	token_t name = cmp->prev;
	
	if (match(cmp, T_LPAREN)) {
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
	emit_value(cmp, newnil());
}

void comp_copystate(
	comp_t* dst, const comp_t* src
) {
	dst->pos = src->pos;
	dst->line = src->line;
	dst->prev = src->prev;
	dst->cur = src->cur;
}

void comp_setfullname(
	comp_t* cmp,
	const string_t* a,
	const string_t* b)
{
	string_t* full = string_empty(cmp->ctx);
	string_append(&full, a, cmp->ctx);
	string_append_lit(&full, "/", cmp->ctx);
	string_append(&full, b, cmp->ctx);
	cmp->func->chunk.fullname = full;
}

/* parse a function declaration
 * in the form of
 * let name(p1, p2, ..) = body */
void parse_func(comp_t* cmp, token_t name) {
	/* add symbol first in order
	 * to support recursion */
	add_symbol(cmp, name);
	
	string_t* funcname = string_new(
		name.start,
		name.len,
		false, 
		cmp->ctx);
	
	/* setup a new compiler instance
	* in order to parse the function */
	comp_t fcmp;
	comp_init(&fcmp, 
		cmp->src,
		funcname,
		cmp->ctx);
	comp_copystate(&fcmp, cmp);
	comp_setfullname(
		&fcmp,
		cmp->func->chunk.fullname,
		fcmp.func->chunk.name);
	fcmp.parent = cmp;
		
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
		"expected ')' or a parameter "
		"name");
	
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
			
		chunk_write(&cmp->func->chunk,
			upval->islocal,
			cmp->line,
			cmp->ctx);
		chunk_write(&cmp->func->chunk,
			upval->index,
			cmp->line,
			cmp->ctx);
	}
		
	comp_copystate(cmp, &fcmp);
	comp_free(&fcmp);
}

/* parses an if/else expression */
void if_else(comp_t* cmp) {
	/* compile condition */
	expr(cmp, ANY_PREC);
	eat(cmp, T_LCURLY,
		"expected '{' after condition");
	
	/* optionally jump to else branch
	 * if condition is false */
	int then_addr =
		emit_jump(cmp, OP_JMPIFN);
	emit_nullary(cmp, OP_POP);
	/* condition was true, pop it and
	 * stay on the then branch */
		
	block(cmp);
	
	/* unconditionally jump past the 
	 * else branch */
	int else_addr =
		emit_jump(cmp, OP_JMP);
	
	patch_jump(cmp, then_addr);
	emit_nullary(cmp, OP_POP);
	/* condition was false so we skipped
	 * to the else branch. make sure
	 * we pop the condition here */
	
	if (match(cmp, T_ELSE)) {
		eat(cmp, T_LCURLY,
			"expected '{' after 'else'");
		block(cmp);
	
	} else {
		/* make sure we always leave a
		 * value on the stack, even if
		 * the condition evaluates
		 * to false and we don't have
		 * an 'else' branch */
		emit_value(cmp, newnil());
	}
	
	patch_jump(cmp, else_addr);
	/* skipped past else */
}

/* parses a block expression.
 * in essence, a block is a series of
 * expressions ultimately reduced down
 * to a single value */
void block(comp_t* cmp) {
	cmp->depth++;
	
	/* a block must return a value even
	 * when it contains zero expressions */
	if (match(cmp, T_RCURLY)) {
		emit_value(cmp, newnil());
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
	emit_nullary(cmp, OP_HIDERET);
	
	/* pop the locals */
	while (cmp->nnames > 0 &&
		cmp->names[cmp->nnames - 1].depth
			> cmp->depth)
	{
		symbol_t* name =
			&cmp->names[cmp->nnames - 1];
		cmp->nnames--;
		
		if (name->capped)
			emit_nullary(cmp, OP_MOVEHEAP);
		else
			emit_nullary(cmp, OP_POP);
	}
	
	/* restore the return value */
	emit_nullary(cmp, OP_SHOWRET);
	
	/* shrink symbol table */
	cmp->names = arr_resize(
		cmp->names,
		symbol_t,
		cmp->nnames + locals,
		cmp->nnames,
		cmp->ctx);
}

func_t* compile(comp_t* cmp) {
	cmp->ctx->state = STATE_COMPILING;
	printf("compiling...\n");
	
	#if DBG_PRINT_SOURCE
	puts((char*)cmp->src->bytes);
	#endif
	
	/* scan initial token for lookahead */
	cmp->cur = scan(cmp);
	
	/* parse the entire source */
	while (!check(cmp, T_EOF)) {
		expr(cmp, ANY_PREC);
		emit_nullary(cmp, OP_POP);
	}
	
	printf("done\n");
	emit_nullary(cmp, OP_RET);
	
	cmp->ctx->state = STATE_COMPILED;
	return cmp->func;
}

void slib_add(
	sylt_t* ctx,
	const char* name,
	cfunc_t cfunc,
	int params)
{
	token_t token =
		(token_t){T_NAME, name, strlen(name)};
	add_symbol(ctx->cmp, token);
	
	func_t* func = func_newc(
		ctx,
		string_lit(name, ctx),
		cfunc,
		params);
	emit_value(ctx->cmp, wrapfunc(func));
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
	
	char* bytes =
		arr_alloc(char, len + 1, ctx);
	
	fread(bytes, 1, len, fp);
	bytes[len] = '\0';
	fclose(fp);
	
	return string_new(
		bytes, len + 1, true, ctx);
}

/* halts with an error message */
void halt(
	sylt_t* ctx,
	int code,
	const char* fmt, ...)
{
	char msg[1024];
	
	va_list args;
	va_start(args, fmt);
	vsnprintf(msg, 1024, fmt, args);
	va_end(args);
	
	/* print error message */
	if (ctx->state == STATE_COMPILING) {
		const chunk_t* chunk =
			&ctx->cmp->func->chunk;
		printf("error in ");
		string_print(chunk->fullname);
		printf(":%d: ", ctx->cmp->line);
		
	} else if (ctx->state == STATE_RUNNING) {
		const chunk_t* chunk =
			&ctx->vm->fp->func->chunk;
		
		size_t addr = (size_t)
			(ctx->vm->fp->ip -
				chunk->code - 1);
		int32_t line = chunk->lines[addr];
		
		printf("error in ");
		string_print(chunk->fullname);
		printf(":%d: ", line);
		
	} else {
		printf("error: ");
	}
	
	printf("%s\n", msg);
	exit(EXIT_FAILURE);
}

sylt_t* sylt_new(void) {
	sylt_t* ctx = ptr_alloc(sylt_t, NULL);
	ctx->state = STATE_INIT;
	ctx->cmp = ptr_alloc(comp_t, ctx);
	ctx->vm = ptr_alloc(vm_t, ctx);
	ctx->mem.objs = NULL;
	/* has to be done manually when
	 * allocating ctx struct itself */
	ctx->mem.bytes += sizeof(sylt_t);
	return ctx;
}

void sylt_free(sylt_t* ctx) {
	obj_t* obj = ctx->mem.objs;
	while (obj) {
		obj_t* next = obj->next;
		obj_free(obj, ctx);
		obj = next;
	}
	
	ptr_free(ctx->cmp, comp_t, ctx);
	ptr_free(ctx->vm, vm_t, ctx);
	
	ptrdiff_t bytesleft =
		ctx->mem.bytes - sizeof(sylt_t);
	if (bytesleft)
		printf("%ld bytes leaked\n",
			bytesleft);
	assert(!bytesleft);
	
	ptr_free(ctx, sylt_t, ctx);
}

void sylt_dofile(
	sylt_t* ctx,
	const char* path)
{
	string_t* src = load_file(path, ctx);
	string_t* src_name = string_new(
		path, strlen(path), false, ctx);
	
	comp_init(ctx->cmp, src, src_name, ctx);
	load_slib(ctx);
	func_t* func = compile(ctx->cmp);
	comp_free(ctx->cmp);
	
	vm_init(ctx->vm, ctx);
	vm_exec(ctx->vm, func);
	vm_free(ctx->vm);
}

int main(int argc, char *argv[]) {
	#if DBG_PRINT_ALLOCS
	printf("sizeof(list_t) = %ld\n",
		sizeof(list_t));
	printf("sizeof(string_t) = %ld\n",
		sizeof(string_t));
	printf("sizeof(func_t) = %ld\n",
		sizeof(func_t));
	printf("sizeof(closure_t) = %ld\n",
		sizeof(closure_t));
	printf("sizeof(upvalue_t) = %ld\n",
		sizeof(upvalue_t));
	#endif
	
	if (argc == 1) {
		printf("Usage: sylt [file]\n");
		return EXIT_SUCCESS;
	}
	
	sylt_t* ctx = sylt_new();
	sylt_dofile(ctx, argv[1]);
	sylt_free(ctx);
	
	return EXIT_SUCCESS;
}
