# sylt

early work in progress, see tests.sylt for example code

virtual machine:
- executes bytecode
- two stacks, one for operands and one for functions
- dynamically typed with gradual typing planned
- garbage collector manages memory

types:
```
Nil
Bool
Num
List
String
Func
```

operators:
```
+ (addition)
- (subtraction / unary minus)
* (multiplication)
/ (division)
% (remainder)
< (less than)
<= (less than or equal to)
> (greater than)
>= (greater than or equal to)
= (is equal to)
!= (is not equal to)
! (boolean not)
and (boolean and)
or (boolean or)
```


