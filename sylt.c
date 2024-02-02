#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* ==== debug flags ==== */

#define DBG_PRINT_SOURCE 1
#define DBG_PRINT_TOKENS 0
#define DBG_PRINT_NAMES 1
#define DBG_PRINT_POOL 1
#define DBG_PRINT_OPCODES 1
#define DBG_PRINT_STACK 1
#define DBG_PRINT_ALLOCS 0
#define DBG_CHECK_MEMLEAKS 1
#define DBG_ASSERT 1

/* ==== optimization flags ==== */

#define OPTZ_DEDUP_CONSTANTS 1
#define OPTZ_EMIT_DUP 1
#define OPTZ_PUSHPOP 1

/* ==== code limits ==== */

#define CHUNK_MAX_CODE (UINT16_MAX + 1)
#define CHUNK_MAX_POOL (UINT8_MAX + 1)
#define JUMP_MAX_DISTANCE UINT16_MAX
#define STACK_MAX_SLOTS (UINT8_MAX + 1)
#define MAX_CFRAMES 64
#define MAX_PARAMS UINT8_MAX

/* ==== debug macros ==== */

#if DBG_ASSERT
#define deverr(msg) \
	printf("%s in %s:%d", \
		(msg), __FILE__, __LINE__); \
	exit(EXIT_FAILURE);
#define assert(cond) \
	if (!(cond)) { \
		deverr("assertion failed"); \
	}
#define unreachable() \
	deverr("unreachable code entered");
#else
#define deverr(msg)
#define assert(cond)
#define unreachable()
#endif

/* sylt API struct */
typedef struct {
	/* compiler */
	struct comp_s* cmp;
	/* virtual machine */
	struct vm_s* vm;
	/* linked list of all objects */
	struct obj_s* objs;
	/* heap size */
	ptrdiff_t memuse;
} sylt_t;

/* ==== error code + message macros ==== */

#define E_OUTOFMEM \
	1, "out of memory"
#define E_OUTOFBOUNDS(msg, i, n) \
	2, "%s (%d/%d)", (msg), (i), (n)
#define E_IO(msg, path) \
	3, "%s %s", (msg), (path)
#define E_DIVBYZERO \
	4, "division by zero"
#define E_UNEXPECTEDCHAR(c) \
	5, "unknown character '%c'", (c)
#define E_SYNTAX(msg) \
	6, "%s", (msg)
#define E_TYPE(ex, got) \
	7, "expected %s but got %s", \
		TYPENAME[(ex)], TYPENAME[(got)]
#define E_UNDEFINED(name, len) \
	8, "undefined variable '%.*s'", \
		(name), (len)
#define E_ESCAPESEQ(code) \
	9, "unknown escape sequence '\\%c'", \
		(code)
#define E_UNTERMSTRING \
	10, "unterminated string"
#define E_TOOMANYPARAMS(max) \
	11, "too many parameters; limit is %d", \
		(max)
#define E_TOOMANYARGS(max) \
	12, "too many arguments; limit is %d", \
		(max)
#define E_WRONGARGC(need, got) \
	13, "expected %d arguments but got %d", \
		(need), (got)
#define E_STACKOVERFLOW \
	14, "stack overflow"

/* halts with an error message,
 * use the above macros as arguments */
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
	
	printf("error: %s\n", msg);
	exit(EXIT_FAILURE);
}

/* ==== memory ==== */

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
		ctx->memuse += ns - os;
	
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

/* ==== opcodes ==== */

typedef enum {
	/* stack */
	OP_PUSH,
	OP_POP,
	OP_DUP,
	OP_LOAD,
	OP_STORE,
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
	[OP_POP] = {"pop", 0, -1},
	[OP_DUP] = {"dup", 0, +1},
	[OP_LOAD] = {"load", 1, +1},
	[OP_STORE] = {"store", 1, 0},
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
	struct value_s* pool;
	size_t npool;
	/* total stack slots needed */
	size_t slots;
	/* name, for errors and debugging */
	struct string_s* name;
} chunk_t;

/* value types */
typedef enum {
	TYPE_NIL,
	TYPE_BOOL,
	TYPE_NUM,
	TYPE_STRING,
	TYPE_FUNCTION,
} type_t;

static const char* TYPENAME[] = {
	"Nil", "Bool", "Num", "String", "Function"
};

#define isheaptype(t) \
	((t) == TYPE_STRING \
		|| (t) == TYPE_FUNCTION)

/* heap-allocated object header */
typedef struct obj_s {
	type_t tag;
	/* linked list of all objects */
	struct obj_s* next;
} obj_t;

/* string object */
typedef struct string_s {
	obj_t obj;
	/* list of characters, not required
	 * to be zero-terminated */
	uint8_t* bytes;
	size_t len;
} string_t;

/* function pointer to a sylt library
 * function */
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
} func_t;

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
	obj->next = ctx->objs;
	ctx->objs = obj;
	return obj;
}

void obj_free(obj_t* obj, sylt_t* ctx) {
	switch (obj->tag) {
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
	default: unreachable();
	}
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

void string_print(string_t* str) {
	printf("%.*s", (int)str->len, str->bytes);
}

/* creates a new function */
func_t* func_new(
	sylt_t* ctx, string_t* name
) {
	func_t* func = (func_t*)obj_new(
		sizeof(func_t), TYPE_FUNCTION, ctx);
	
	chunk_init(&func->chunk, name);
	func->cfunc = NULL;
	func->params = 0;
	return func;
}

/* creates a new C function */
func_t* func_newc(
	sylt_t* ctx,
	string_t* name,
	cfunc_t cfunc,
	int params
) {
	func_t* func = func_new(ctx, name);
	func->cfunc = cfunc;
	func->params = params;
	return func;
}

/* tagged enum representing a sylt value */
typedef struct value_s {
	type_t tag;
	union {
		double num;
		obj_t* obj;
	} data;
} value_t;

/* macros for creating a value_t
 * from a raw value */
#define newnil() \
	(value_t){TYPE_NIL, {.num = 0}}
#define newbool(v) \
	(value_t){TYPE_BOOL, {.num = (v)}}
#define newnum(v) \
	(value_t){TYPE_NUM, {.num = (v)}}
#define newstring(v) \
	(value_t){TYPE_STRING, \
		{.obj = (obj_t*)(v)}}
#define newfunc(v) \
	(value_t){TYPE_FUNCTION, \
		{.obj = (obj_t*)(v)}}

/* macros for reading raw values from 
 * value_t's. the type must be
 * checked before accessing these to
 * prevent undefined behaviour */
#define getbool(v) (v).data.num
#define getnum(v) (v).data.num
#define getobj(v) (v).data.obj
#define getstring(v) \
	((string_t*)(v).data.obj)
#define getfunc(v) \
	((func_t*)(v).data.obj)

#define typecheck(ctx, v, t) \
	if (v.tag != t) \
		halt(ctx, E_TYPE(t, v.tag))
#define typecheck2(ctx, a, b, t) \
	typecheck(ctx, a, t); \
	typecheck(ctx, b, t)

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
	case TYPE_STRING: {
		string_t* astr = getstring(a);
		string_t* bstr = getstring(b);
		return astr->len == bstr->len
			&& !memcmp(
				astr->bytes,
				bstr->bytes,
				astr->len);
	}
	case TYPE_FUNCTION: {
		return getobj(a) == getobj(b);
	}
	default: unreachable();
	}
}

/* converts a value to a printable string */
string_t* val_tostring(
	value_t val, sylt_t* ctx
) {
	switch (val.tag) {
	case TYPE_NIL:
		return string_new(
			"nil", 3, false, ctx);
	case TYPE_BOOL:
		if (getbool(val))
			return string_new(
				"true", 4, false, ctx);
		else
			return string_new(
				"false", 5, false, ctx);
	case TYPE_NUM: {
		size_t len = snprintf(
			NULL, 0, "%g", getnum(val)) + 1;
		char* bytes =
			arr_alloc(char, len, ctx);
		snprintf(
			bytes, len, "%g", getnum(val));
		
		return string_new(
			bytes, len, true, ctx);
	}
	case TYPE_STRING: {
		return getstring(val);
	}
	case TYPE_FUNCTION: {
		return getfunc(val)->chunk.name;
	}
	default: unreachable();
	}
	
	return NULL;
}

/* prints a value to stdout */
void val_print(
	value_t val,
	bool quotestr,
	int maxlen,
	sylt_t* ctx
) {
	string_t* str =
		val_tostring(val, ctx);
	
	if (quotestr && val.tag == TYPE_STRING) {
		int len = str->len;
		bool shortened = false;
		
		if (maxlen > 0 && len > maxlen) {
			len = maxlen;
			shortened = true;
		}
	
		printf("'%.*s'", len, str->bytes);
		
		if (shortened)
			printf("(+%ld)",
				str->len - maxlen);
	} else {
		printf("%.*s",
			(int)str->len, str->bytes);
		
		if (val.tag == TYPE_FUNCTION)
			printf("()");
	}
}

void chunk_init(
	chunk_t* chunk, string_t* name
) {
	chunk->code = NULL;
	chunk->ncode = 0;
	chunk->pool = NULL;
	chunk->npool = 0;
	chunk->slots = 0;
	chunk->name = name;
}

void chunk_free(
	chunk_t* chunk,
	sylt_t* ctx)
{
	arr_free(
		chunk->code, uint8_t, chunk->ncode,
		ctx);
    arr_free(
    	chunk->pool, value_t, chunk->npool,
    	ctx);
    chunk_init(chunk, NULL);
}

/* writes a byte to the code array */
void chunk_write(
	chunk_t* chunk,
	uint8_t byte,
	sylt_t* ctx)
{
	if (chunk->ncode >= CHUNK_MAX_CODE) {
		halt(ctx, E_OUTOFBOUNDS(
			"chunk bytecode limit reached",
			chunk->ncode,
			CHUNK_MAX_CODE));
		return;
	}
	
	chunk->code = arr_resize(
		chunk->code,
		uint8_t,
		chunk->ncode,
		chunk->ncode + 1,
		ctx);
	chunk->code[chunk->ncode++] = byte;
}

/* writes a value to the constant pool
 * and returns the index it was written to */
size_t chunk_write_pool(
	chunk_t* chunk, value_t val, sylt_t* ctx)
{
	#if OPTZ_DEDUP_CONSTANTS
	/* check if a constant with the same
	 * value already exists and if so
	 * return its index */
	for (
		size_t i = 0; i < chunk->npool; i++)
	{
		if (val_eq(val, chunk->pool[i]))
			return i;
	}
	#endif
	
	if (chunk->npool >= CHUNK_MAX_POOL) {
		halt(ctx, E_OUTOFBOUNDS(
			"chunk constant limit reached",
			chunk->npool,
			CHUNK_MAX_POOL));
		return SIZE_MAX;
	}
		
	chunk->pool = arr_resize(
		chunk->pool,
		value_t,
		chunk->npool,
		chunk->npool + 1,
		ctx);
	
	chunk->pool[chunk->npool++] = val;
	return chunk->npool - 1;
}

/* execution state relative to the last
 * function call */
typedef struct {
	/* prototype of called function */
	const func_t* func;
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
	/* reference to API */
	sylt_t* ctx;
} vm_t;

void vm_init(vm_t* vm, sylt_t* ctx) {
	vm->nframes = 0;
	vm->fp = NULL;
	vm->stack = NULL;
	vm->maxstack = 0;
	vm->sp = NULL;
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
	const vm_t* vm, const func_t* func)
{
	const chunk_t* chunk = &func->chunk;
	
	printf("\n---- FRAME ----\n");
	printf("depth %d/%d, ",
		(int)vm->nframes, MAX_CFRAMES);
	
	size_t used = vm->sp - vm->stack;
	printf("stack usage %ld/%ld\n",
		used, vm->maxstack);
	
	printf("name: ");
	string_print(chunk->name);
	putchar('\n');
	
	printf("%ld bytes, ", chunk->ncode);
	printf("%ld constants, ", chunk->npool);
	printf("%ld stack slots\n", chunk->slots);
	
	#if DBG_PRINT_POOL
	puts("  constants:");
	for (size_t i = 0; i < chunk->npool; i++)
	{
		printf("    #%-2ld -- ", i);
		val_print(chunk->pool[i],
			true, 24, vm->ctx);
		putchar('\n');
	}
	#endif
		
	puts("  addr opcode             hex");
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
		(size_t)(vm->fp->ip
			- chunk->code - 1);
	const char* name = OPINFO[op].name;
	printf("  %05ld %-18s",
		addr, name);
		
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
	const int maxvals = 4;
	
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
	
	printf("  [ ");
	for (; v != vm->sp; v++) {
		val_print(*v, true, 12, vm->ctx);
		if (v != vm->sp - 1)
		  printf(", ");
	}
	
	if (diff > maxvals)
		printf(" ](+%ld)\n", diff - maxvals);
	else
		printf(" ]\n");
}

/* pushes a value on the sylt stack */
inline static void sylt_push(
	vm_t* vm, value_t val)
{
	*vm->sp++ = val;
}

/* pops a value from the sylt stack */
inline static value_t sylt_pop(vm_t* vm) {
	return *(--vm->sp);
}

inline static value_t sylt_peek(
	vm_t* vm, int depth
) {
	//assert(depth > 0);
	return vm->sp[-1 - (depth)];
}

/* virtual machine macros */
#define read() *vm->fp->ip++
#define read16() \
	(vm->fp->ip += 2, (uint16_t) \
		((vm->fp->ip[-2] << 8) \
			| vm->fp->ip[-1]))
#define push(v) sylt_push(vm, v)
#define pop() sylt_pop(vm)
#define peek(d) sylt_peek(vm, d)

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

void vm_exec(vm_t* vm, const func_t* entry) {
	/* init stack */
	vm_growstack(vm, entry->chunk.slots);
	
	/* init stack pointer */
	vm->sp = vm->stack;
	
	/* setup first call frame */
	cframe_t frame;
	frame.func = entry;
	frame.ip = entry->chunk.code;
	frame.offs = 0;
	vm->frames[vm->nframes++] = frame;
	vm->fp = &vm->frames[vm->nframes - 1];
	
	#if DBG_PRINT_OPCODES
	dbg_print_header(vm, vm->fp->func);
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
			value_t val =
				vm->fp->func->
					chunk.pool[read()];
			push(val);
			break;
		}
		case OP_POP: {
			value_t val = pop();
			putchar('=');
			string_t* str =
				val_tostring(val, vm->ctx);
			string_print(str);
			putchar('\n');
			break;
		}
		case OP_DUP: {
			value_t val = peek(0);
			push(val);
			break;
		}
		case OP_LOAD: {
			value_t val = *(vm->stack
				+ vm->fp->offs
				+ read());
			push(val);
			break;
		}
		case OP_STORE: {
			value_t val = vm->sp[-1];
			*(vm->stack
				+ vm->fp->offs
				+ read()) = val;
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
				vm->sp[-1],
				TYPE_NUM);
			vm->sp[-1].data.num =
				-vm->sp[-1].data.num;
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
			vm->sp[-1].data.num =
				!vm->sp[-1].data.num;
			break;
		}
		/* control flow */
		case OP_JMP: {
			uint16_t offset = read16();
			vm->fp->ip += offset;
			break;
		}
		case OP_JMPIFN: {
			uint16_t offset = read16();
			typecheck(
				vm->ctx,
				peek(0),
				TYPE_BOOL);
			if (!getbool(peek(0)))
				vm->fp->ip += offset;
			break;
		}
		case OP_CALL: {
			uint8_t argc = read();
			func_t* func =
				getfunc(peek(argc));
				
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
				printf("---- CFUNC ----\n");
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
			frame.ip = func->chunk.code;
			frame.offs = used - argc;
			vm->frames[vm->nframes++] = frame;
			vm->fp = &vm->frames[
				vm->nframes - 1];
				
			#if DBG_PRINT_OPCODES
			dbg_print_header(vm,
				vm->fp->func);
			#endif
			
			break;
		}
		case OP_RET: {
			value_t result = pop();
			
			vm->nframes--;
			if (vm->nframes == 0)
				return;
				
			vm->sp =
				vm->stack + vm->fp->offs - 1;
			push(result);
			
			vm->fp = &vm->frames[
				vm->nframes - 1];
			
			#if DBG_PRINT_OPCODES
			dbg_print_header(
				vm, vm->fp->func);
			#endif
			break;
		}
		default: unreachable(); return;
		}
	}
}

void vm_math(vm_t* vm, op_t opcode) {
	typecheck(vm->ctx, vm->sp[-1], TYPE_NUM);
	double b = getnum(pop());
	
	typecheck(vm->ctx, vm->sp[-1], TYPE_NUM);
	double a = getnum(pop());
	
	switch (opcode) {
	/* arithmetic */
	case OP_ADD: push(newnum(a + b)); break;
	case OP_SUB: push(newnum(a - b)); break;
	case OP_MUL: push(newnum(a * b)); break;
	case OP_DIV:
		if (b == 0.0f) {
			halt(vm->ctx, E_DIVBYZERO);
			push(newnum(0.0f));
		}
		push(newnum(a / b));
		break;
	case OP_EDIV: {
		if (b == 0.0f) {
			halt(vm->ctx, E_DIVBYZERO);
			push(newnum(0.0f));
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

/* ==== sylt standard library ==== */

#define argc() (ctx->vm->fp->ip[-1])
#define arg(n) (*(ctx->vm->sp - argc()))

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
	return newnil();
}

void slib_add(
	sylt_t* ctx,
	const char* name,
	cfunc_t cfunc,
	int params);

void load_slib(sylt_t* ctx) {
	slib_add(ctx, "put", slib_put, 1);
	slib_add(ctx, "ensure", slib_ensure, 1);
}

typedef enum {
	T_NAME,
	T_NIL,
	T_TRUE,
	T_FALSE,
	T_LET,
	T_IF,
	T_ELSE,
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
	PREC_EQ,
	PREC_CMP,
	PREC_TERM,
	PREC_FACTOR,
	PREC_UNARY,
	PREC_CALL,
	PREC_PRIMARY,
} prec_t;

const int ANY_PREC = PREC_ASSIGN;

typedef struct {
	const char* name;
	size_t len;
	/* scope depth */
	int depth;
} symbol_t;

/* compiler state */
typedef struct comp_s {
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
	size_t line;
	/* parsing tokens */
	token_t prev;
	token_t cur;
	/* symbol table */
	symbol_t* names;
	size_t nnames;
	/* current scope depth */
	int depth;
	/* reference to API */
	sylt_t* ctx;
} comp_t;

void comp_init(
	comp_t* cmp,
	string_t* src,
	string_t* src_name,
	sylt_t* ctx)
{
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

/* helper function to get the output chunk */
chunk_t* comp_chunk(comp_t* cmp) {
	return &cmp->func->chunk;
}

/* should not be used directly; use the
 * emit_ functions below instead */
void emit_op(
	comp_t* cmp, op_t op)
{
	chunk_t* chunk = comp_chunk(cmp);
	
	/* keep track of how much stack
	 * space we're using */
	cmp->curslots += OPINFO[op].effect;
	assert(cmp->curslots >= 0);
	assert(cmp->curslots <= STACK_MAX_SLOTS);
	
	/* record the largest stack size */
	if (cmp->curslots > chunk->slots)
		chunk->slots = cmp->curslots;
	
	#if OPTZ_PUSHPOP
	/* prevents emitting code when pushing
	 * a value and immediately popping it */
	bool waspush = cmp->lastop == OP_PUSH;
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
		if (cmp->lastop == OP_PUSH) {
			chunk->pool = arr_resize(
				chunk->pool,
				value_t,
				chunk->npool,
				chunk->npool - 1,
				cmp->ctx);
			chunk->npool--;
		}
		
		cmp->lastop = -1;
		return;
	}
	#endif
	
	/* write the instruction opcode */
	chunk_write(
		comp_chunk(cmp), op, cmp->ctx);
	cmp->lastop = op;
}

/* emits an instruction with no operands */
void emit_nullary(
	comp_t* cmp, op_t op)
{
	assert(OPINFO[op].rank == 0);
	emit_op(cmp, op);
}

/* emits an instruction with one operand */
void emit_unary(
	comp_t* cmp, op_t op, uint8_t arg)
{
	assert(OPINFO[op].rank == 1);
	emit_op(cmp, op);
	chunk_write(
		comp_chunk(cmp), arg, cmp->ctx);
}

/* emits an instruction with two operands */
void emit_binary(
	comp_t* cmp,
	op_t op,
	uint8_t a,
	uint8_t b)
{
	assert(OPINFO[op].rank == 2);
	emit_op(cmp, op);
	chunk_write(comp_chunk(cmp), a, cmp->ctx);
	chunk_write(comp_chunk(cmp), b, cmp->ctx);
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
	#endif
	
	size_t slot = chunk_write_pool(
		comp_chunk(cmp), val, cmp->ctx);
	emit_unary(cmp, OP_PUSH, slot);
	
	cmp->lastpush = val;
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
	if (dist > JUMP_MAX_DISTANCE) {
		halt(cmp->ctx, E_OUTOFBOUNDS(
			"tried to jump too many bytes",
			dist,
			JUMP_MAX_DISTANCE));
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
int add_symbol(
	comp_t* cmp, const char* name, size_t len)
{
	#if DBG_PRINT_NAMES
	for (int i = 0; i < cmp->depth; i++)
		printf("  ");
	printf("(sym) %.*s\n", (int)len, name);
	#endif
	
	symbol_t sym;
	sym.name = name;
	sym.len = len;
	sym.depth = cmp->depth;
	
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
int find_symbol(
	comp_t* cmp, const char* name, size_t len)
{
	if (cmp->nnames == 0)
		return -1;
	
	/* search backwards in case of
	 * shadowed variable names */
	int start = cmp->nnames - 1;
	for (int i = start; i >= 0; i--) {
		symbol_t* other = &cmp->names[i];
		bool equal = other->len == len
			&& memcmp(other->name, name, len)
				== 0;
		if (equal)
			return i;
	}
	
	return -1;
}

void slib_add(
	sylt_t* ctx,
	const char* name,
	cfunc_t cfunc,
	int params)
{
	comp_t* cmp = ctx->cmp;
	add_symbol(cmp, name, strlen(name));
	
	func_t* func = func_newc(
		ctx,
		string_new(
			name,
			strlen(name),
			false,
			ctx),
		cfunc,
		params);
	emit_value(cmp, newfunc(func));
}

/* scans the source code for the next token */
token_t scan(comp_t* cmp) {
	#define token(t) \
		(token_t){t, start, cmp->pos - start}
	#define step() cmp->pos++
	#define peek() (*cmp->pos)
	#define eof() (peek() == '\0')
	#define match(c) \
		((!eof() && peek() == (c)) ? \
			step(), true : false)
	
	/* skip any initial whitespace */
	for (;;) {
		if (!isspace(peek())) {
			if (peek() == '\n')
				cmp->line++;
			break;
		}
		
		step();
	}
	
	/* remember first non-whitespace char */
	const char* start = cmp->pos;
	if (eof())
		return token(T_EOF);
	
	/* symbol name or keyword */
	if (isalpha(peek()) || peek() == '_') {
		while (isalnum(peek())
			|| peek() == '_')
			step();
		
		if (!strncmp(start, "nil", 3))
			return token(T_NIL);
		if (!strncmp(start, "true", 4))
			return token(T_TRUE);
		if (!strncmp(start, "false", 5))
			return token(T_FALSE);
		if (!strncmp(start, "let", 3))
			return token(T_LET);
		if (!strncmp(start, "if", 2))
			return token(T_IF);
		if (!strncmp(start, "else", 4))
			return token(T_ELSE);
		
		return token(T_NAME);
	}
	
	/* string literal */
	if (peek() == '"') {
		step();
		while (
			(peek() != '"'
				|| cmp->pos[-1] == '\\')
			&& !eof())
		{
			step();
		}
		
		if (eof()) {
			halt(cmp->ctx,
				E_UNTERMSTRING);
		}
		
		step();
		return token(T_STRING);
	}
	
	/* numbers begin with a digit */
	if (isdigit(peek())) {
		while (isdigit(peek()))
			step();
		
		if (peek() == '.') {
			step();
			while (isdigit(peek()))
				step();
		}
			
		return token(T_NUMBER);
	}
	
	/* see if we can find an operator */
	switch (*cmp->pos++) {
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
	case ',': return token(T_COMMA);
	}
	
	halt(cmp->ctx,
		E_UNEXPECTEDCHAR(cmp->pos[-1]));
	return token(-1);
	
	#undef token
	#undef step
	#undef peek
	#undef eof
	#undef match
}

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
void string(comp_t*);
void number(comp_t*);
void grouping(comp_t*);
void unary(comp_t*);
void binary(comp_t*);
void call(comp_t*);
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
		halt(cmp->ctx, E_SYNTAX(
			"expected expression"));
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
	int index = find_symbol(
		cmp,
		cmp->prev.start,
		cmp->prev.len);
	
	if (index == -1) {
		halt(cmp->ctx, E_UNDEFINED(
			cmp->prev.len,
			cmp->prev.start));
		return;
	}
	
	#if OPTZ_EMIT_DUP
	if (index == cmp->lastload) {
		emit_nullary(cmp, OP_DUP);
		return;
	}
	#endif
	
	emit_unary(cmp, OP_LOAD, index);
	cmp->lastload = index;
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
	emit_value(cmp, newstring(strobj));
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
	expr(cmp, rule->prec + 1);
	
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
	default: unreachable();
	}
	
	emit_nullary(cmp, opcode);
}

void call(comp_t* cmp) {
	int argc = 0;
	
	/* parse argument list */
	while (
		!check(cmp, T_RPAREN)
		&& !check(cmp, T_EOF)
	) {
		if (argc >= MAX_PARAMS) {
			halt(cmp->ctx, 
				E_TOOMANYARGS(
					MAX_PARAMS));
			unreachable();
		}
			
		expr(cmp, ANY_PREC);
		argc++;
			
		if (!match(cmp, T_COMMA))
			break;
	}
	eat(cmp, T_RPAREN, "expected ')'");
	
	emit_unary(cmp, OP_CALL, argc);
}

void comp_copystate(
	const comp_t* from, comp_t* to
) {
	to->pos = from->pos;
	to->line = from->line;
	to->prev = from->prev;
	to->cur = from->cur;
}

/* parses a variable declaration */
void let(comp_t* cmp) {
	eat(cmp, T_NAME,
		"expected variable name after 'let'");
	
	/* remember the name token */
	token_t name = cmp->prev;
	
	if (match(cmp, T_LPAREN)) {
		/* parse a function declaration
		 * in the form of
		 * let name(p1, p2, ..) = body */
		 
		/* setup a new compiler instance
		 * in order to parse the function */
		comp_t fcmp;
		comp_init(
			&fcmp,
			cmp->src,
			string_new(
				name.start,
				name.len,
				false,
				cmp->ctx),
			cmp->ctx);
		comp_copystate(cmp, &fcmp);
		
		/* parse parameter list */
		while (
			!check(&fcmp, T_RPAREN)
			&& !check(&fcmp, T_EOF)
		) {
			if (fcmp.func->params
				>= MAX_PARAMS)
			{
				halt(fcmp.ctx, 
					E_TOOMANYPARAMS(
						MAX_PARAMS));
				unreachable();
			}
			
			eat(&fcmp, T_NAME,
				"expected parameter name");
			
			token_t name = fcmp.prev;
			add_symbol(
				&fcmp,
				name.start,
				name.len);
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
		
		/* push the function on the stack */
		emit_value(cmp, newfunc(fcmp.func));
			
		comp_copystate(&fcmp, cmp);
		comp_free(&fcmp);
		
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
	}
	
	add_symbol(cmp, name.start, name.len);
	
	/* expression yields a 'nil' */
	emit_value(cmp, newnil());
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
 * to a single value, its "return value" */
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
	 
	/* it's time to shrink the stack in order
	 * to get rid of any lingering local
	 * variables; we start by counting
	 * how many there are while also deleting
	 * their names from the symbol table */
	int locals = 0;
	while (cmp->nnames > 0 &&
		cmp->names[cmp->nnames - 1].depth
			> cmp->depth)
	{
		locals++;
		cmp->nnames--;
	}
	
	if (locals > 0) {
		/* shrink symbol table */
		cmp->names = arr_resize(
			cmp->names,
			symbol_t,
			cmp->nnames + locals,
			cmp->nnames,
			cmp->ctx);
		
		/* move the result of the expression
		 * to a safe place on the stack */
		int safe =
			cmp->curslots - locals - 1;
		emit_unary(cmp, OP_STORE, safe);
		
		/* pop the locals */
		for (size_t i = 0; i < locals; i++)
			emit_nullary(cmp, OP_POP);
	}
}

func_t* compile(comp_t* cmp) {
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
	return cmp->func;
}

string_t* load_file(
	const char* path, sylt_t* ctx)
{
	FILE* fp = fopen(path, "rb");
	if (!fp) {
		halt(ctx, E_IO(
			"failed to open %s", path));
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

sylt_t* sylt_new(void) {
	sylt_t* ctx =
		ptr_alloc(sylt_t, NULL);
	ctx->cmp = ptr_alloc(comp_t, ctx);
	ctx->vm = ptr_alloc(vm_t, ctx);
	ctx->objs = NULL;
	/* has to be done manually when
	 * allocating ctx struct itself */
	ctx->memuse += sizeof(sylt_t);
	return ctx;
}

void sylt_free(sylt_t* ctx) {
	/* free all objects */
	obj_t* obj = ctx->objs;
	for (; obj; obj = obj->next)
		puts("z"), obj_free(obj, ctx);
		
	comp_free(ctx->cmp);
	ptr_free(ctx->cmp, comp_t, ctx);
	
	vm_free(ctx->vm);
	ptr_free(ctx->vm, vm_t, ctx);
	
	ptrdiff_t bytesleft =
		ctx->memuse - sizeof(sylt_t);
	ptr_free(ctx, sylt_t, ctx);
	
	if (bytesleft)
		printf("%ld bytes leaked\n",
			bytesleft);
	assert(!bytesleft);
}

int main(int argc, char *argv[]) {
	#if DBG_PRINT_ALLOCS
	printf("sizeof(string_t) = %ld\n",
		sizeof(string_t));
	printf("sizeof(func_t) = %ld\n",
		sizeof(func_t));
	#endif
	
	sylt_t* ctx = sylt_new();
	string_t* src = NULL;
	string_t* src_name = NULL;
	
	if (argc == 1) {
		char* raw =
			"let x = 20 "
			"let y = 40 "
			"y / x let z = 90";
		src = string_new(
			raw, strlen(raw), false, ctx);
		src_name = string_new(
			"input", 5, false, ctx);
	} else if (argc >= 2) {
		const char* path = argv[1];
		src = load_file(path, ctx);
		src_name = string_new(
			path, strlen(path), false, ctx);
	}
	
	comp_init(ctx->cmp, src, src_name, ctx);
	load_slib(ctx);
	func_t* func = compile(ctx->cmp);
	
	vm_init(ctx->vm, ctx);
	vm_exec(ctx->vm, func);
		
	sylt_free(ctx);
	
	return EXIT_SUCCESS;
}
