# SchemeInterpreter
1. The "src" folder contains separate .scm files each contains procedures of similar functionalities. After some point of the development process, when my interpreter is able to interpret itself for more than one recursive level, I merged all the files into a single file called "interpreter.scm". So the "src" folder may be slightly out-dated but necessarily reflects the key infrastructure of the program.

2. The interpreter is tested under OS X, using command “[Racket Installation Path]/bin/racket” with a call to (load “interpreter.scm”). Excuse me for not using r5rs because it was too late to change fundamental ADTs. The only difference though is between “mcons” and “cons”.

3. The “screenshot” is a picture of how I run an interpreter of an interpreter of an interpreter of the default racket interpreter. I tried to dive into level 4 but failed (took me too long.) The “repl” function takes an optional argument “level” that indicates the recursive level of the current interpreter by displaying longer cursor according to the “level”. call to (repl) implies (repl 1), i.e. level 1. This is inspired by the movie "Inception". Please load in the "library.scm" everytime before diving further.

4. The interpreter is implemented differently from that provided by professor. I decided that the interpreter should support primitive types like char, string, regexp, etc. So I started from scratch. I use lower level io procedures like "read-char", "peek-char" to ensure the input is correctly parsed.

5. I tried to make the behaviors of all pre-defines as much the same of an ordinary scheme interpreter as possible. For example, (= 3 3 4) evaluates to #f. But some of them requires too many control clauses so I decided to give up.

6. I implemented "and" and "or" as special forms because they takes arbitrary number of arguments and don't necessarily evalute all of them. For example, (or #t (display n)) evaluates to #t without displaying "n", regardless of whether "n" is defined or not in that environment.

7. The equal? function and eq? function (though not required), are implemented as built-in functions because after supporting primitive types more than number and pair, at some point I have to determine equality by reference or something, which can't be done if written in "library.scm".

10. "Tail call optimization" is implemented as required for scheme interpreters. The TCO of my interpreter requires that the outer interpreter's TCO. This is achieved by letting special forms like "if", "cond", "begin" and the evaluation process of a lambda function evaluate all but the last expression. For example, under global context, the evaluation of (if #t (+ 1 2) 0) returns a closure of (global-env (+ 1 2)) instead of 3. Therefore if the "eval-expr" function detects that the return value is a closure, it will re-evaluate that closure using a recursive call to itself. Since "eval-expr" is a tail-recursive function, the overall evaluation won't eat up all the frame memory. (Line 78)

9. The "dot notation" feature is implemented so that you can define lambda functions that take arbitrary number of arguments. The "dot notation" for constructing a pair is not yet implemented.

10. Not all features / functions for char, string, regexp are implemented. I implemented the minimum set of functions that either are required by professor or support the self interpretation.

11. The only place I used destructive operators is where I implement the ADT for environment, starts from Line 55. The reason why set-mcar! / set-mcdr! are introduced is that new bindings should be appended to the end of existing bindings without creating a new list (otherwise they're invisible to those referencing that environment before they're defined.) For the same reason the primitive type "mpair" is introduced.

12. Most procedures and pre-defines perform simple validity check on input and raise errors if the input is not considered valid. For the same reason I implemented "with-handlers" and "raise" to support self interpretation since this mechanism is wildly used through my code. This is also the reason why it takes me more than 1000 lines to implement the interpreter (though the boundary check is still too naive.)

13. So although the interpreter has tolerance on bad input, please don't try input that is too bad.

14. The ' notation is implemented as a shorthand of calling "quote". So '(quote 4) evaluates to ''4 while '(quoted 4) just evalutes to '(quoted 4). This behavior really seems weird. Also rebinding "quote" to other values will also change the behavior of '.

15. Following is a list of all pre-defines, including special forms and built-in functions. Built-in functions are not written in "library.scm" because they either manipulate my ADTs or require calling functions of outer interpreter.

special forms:
quote
begin
if
and
or
cond
lambda
define
let
let*
letrec
with-handlers

built-in functions:
exit
void
cons
car
cdr
mcons
mcar
mcdr
set-mcar!
set-mcdr!
apply
raise
+
-
*
/
modulo
floor
=
<
>
eq?
equal?
null?
pair?
false?
number?
real?
char?
string?
regexp?
eof-object?
regexp
regexp-match
list->string
string-length
string->number
string->list
number->string
string-append
display
displayln
newline
load
read
read-char
peek-char
read-line
flush-output
current-input-port
open-input-file
close-input-port
