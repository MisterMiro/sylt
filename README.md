# sylt

A dynamic scripting language inspired mainly by Lua, Rust, and F#. The implementation is written in C99. Early work in progress.



A quick example:
```## prints all even numbers between 1 and 10
let numbers = List.range(0, 10)
let even = List.filter(isEven, numbers)
printLn(even)```



The following types are implemented:
```Nil - no value
Bool - boolean type
Num - numeric type, backed by a float or double
List - a dynamic array
String - an array of UTF-8 bytes
Dict - a dictionary/HashMap
Func - a callable function```



These are the operators:
Arithmetic:
```+ addition 
- subtraction 
* multiplication 
/ division
% remainder of the division of A and B
- (prefix) negates a value```

Comparison:
```<  A less than B?
<= A less than or equal to B?
> A greater than B?
>= A greater than or equal to B?```

Equality:
```= A equal to B?
!= A not equal to B?
! (prefix) inverse of boolean expression
```

Special:
```++ for concatenating lists or strings
```



Standard library:
```Prelude: 
Dict gdict - A dictionary containing all global variables, mainly intended for debugging

Nil print(value) - Prints a value to stdout

Nil printLn(value) - Prints a value and a newline to stdout

String readIn() - Reads input from stdin

String toString(value) - Returns a value of any type converted into a String

Num toNum(value) - Returns a value of any type converted to a Num

Bool floatEq(a, b) - Returns true if a and b are equal within a tolerance of epsilon

String typeOf(value) - Returns the type of a value as a String

Nil ensure(condition) - Halts the program if the condition evaluates to false

Nil todo() - Halts the program if called

Nil unreachable() - Halts the program if called

Dynamic eval(code) - Evaluates an expression contained within a string of code


File:
Num open(filename, mode) - Opens a file in the selected mode and returns a handle

Nil close(handle) - Closes a file handle

Nil write(handle, value) - Writes a string representation of any value to file

String read(handle, nbytes) - Reads n bytes from a file into a String

Num size(handle) - Returns the total size of a file

Nil del(filename) - Deletes a file


System:
String version - Sylt version number

String platform - Operating system

Dict memInfo() - A dictionary containing information about the interpreters memory usage

String memInfoStr() - Returns a human-readable version of memInfo()

Dict memSizes() - A dictionary containing the sizeof() of several C types

Nil halt(message) - Halts execution with an error message

String time(format) - Returns a formatted timestamp using %H-%M-%S notation

Num timestamp() - Returns a numeric timestamp

Num cpuClock() - Returns the CPU time


List:
Num length(list) - Returns the length of the list

Bool isEmpty(list) - Returns true if the list is empty

Nil clear(list) - Empties the list

Nil add(list, index, value) - Inserts a value at the provided index

Nil del(list, index) - Deletes the value at the provided index

Nil push(list, value) - Adds a value to the end of the list

Dynamic pop(list) - Returns and removes the last element of the list

Dynamic first(list) - Returns the first item in the list

Dynamic last(list) - Returns the last item in the list

Num count(list, value) - Returns the number of occurrences a value in the list

Bool contains(list, value) - Returns true if the list contains the given value

List rev(list) - Returns a reversed copy of the list

List range(list, start, end) - Returns a list of the integers between start and end


Dict:
Num length(dict) - Returns the number of keys in the dict

Bool isEmpty(dict) - Returns true if the dict is empty

List keys(dict) - Returns a list containing the dictionaries keys

List values(dict) - Returns a list containing the dictionaries values


String:
String alpha - A lowercase string containing all letters of the English alphabet, a-z

String digits - A string containing all the digits, 0-9 (in case you forgot them)

Num length(string) - Returns the length of the list of bytes stored in a string

List chars(string) - Returns the string converted to a list of bytes

String join(list) - Creates a string by combining a list of values of any type 

List split(string, sep) - Returns a list of strings separated by the string sep

String lower(string) - Returns an all-lowercase version of the string

String upper(string) - Returns an all-uppercase version of the string

String swapCase(string) - Swaps all the lower and uppercase letters in a string

Bool startsWith(string, start) - Returns true if the string starts with another string

Bool endsWith(string, end) - Returns true if the string ends with another string

Bool trimStart(string) - Returns a version of the string with all leading whitespace removed

Bool trimEnd(string) - Returns a version of the string with all trailing whitespace removed

String trim(string) - Trims both the start and end of a string

String replaceAll(string, replace, with) - Replaces all occurrences of a substring with another


Math:
Num pi - The constant pi, π

Num e - Euler's constant

Num numSign(x) - Returns 1 if x is positive, 0 if it is zero, and -1 if negative

Num abs(x) - Returns the absolute (positive) value of a number

Num log(x, base) - Returns the exponent to which a number needs to be raised in order to produce x

Num pow(base, exp) - Returns base raised to the power of exp

Num sqrt(x) - Returns the square root of x

Num min(a, b) - Returns the smaller value of a and b

Num max(a, b) - Returns the larger value of a and b

Num clamp(x, a, b) - Returns x clamped to within the range a..b

Num lerp(a, b, f) - Linearly interpolates between a and b based on f

Num floor(x) - Returns x rounded towards +infinity

Num ceil(x) - Returns x rounded towards -infinity

Num round(x) - Returns the nearest integer value to x, rounding halfway cases away from zero

Num rad(x) - Returns x converted from degrees to radians

Num deg(x) - Returns x converted from radians to degrees

Num sin(x) - Returns the sine of x

Num cos(x) - Returns the cosine of x

Num tan(x) - Returns the tangent of x

Num asin(x) - Returns the arc-sine of x

Num acos(x) - Returns the arc-cosine of x

Num atan(x) - Returns the arc-tangent of x

Num rand(min, max) - Returns a random value between min and max

Num seedRand(x) - Sets the seed of the random number generator used by rand()
```
