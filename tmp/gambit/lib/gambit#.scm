;;;============================================================================

;;; File: "gambit#.scm"

;;; Copyright (c) 2005-2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Identifiers bound to syntactic forms and procedures defined by Gambit
;; are mapped to the empty namespace (no prefix).

(##include "r5rs#.scm") ;; most identifier bindings are inherited from R5RS

(##namespace ("" ;; these identifier bindings are specific to Gambit

;; special forms
define-library
define-module-alias
define-record-type
define-structure
define-type
define-type-of-thread
c-define-type
c-declare
c-initialize
c-lambda
c-define
define-macro
future
import
include
include-ci
declare
namespace
this-source-file
parameterize
receive
time
cond-expand
define-cond-expand-feature
case-lambda
let*-values
let-values
letrec*
letrec*-values
letrec-values
define-values
when
unless
syntax-error
delay-force
guard
r7rs-guard
syntax
syntax-case

;; global variable
default-random-source

;; procedures
abandoned-mutex-exception?
abort
acosh
address-info-family
address-info-protocol
address-info-socket-info
address-info-socket-type
address-info?
address-infos
all-bits-set?
any-bits-set?
append-f32vectors
append-f64vectors
append-s16vectors
append-s32vectors
append-s64vectors
append-s8vectors
append-strings
append-u16vectors
append-u32vectors
append-u64vectors
append-u8vectors
append-vectors
apropos
arithmetic-shift
asinh
atanh
binary-port?
bit-count
bit-set?
bitwise-and
bitwise-ior
bitwise-merge
bitwise-not
bitwise-xor
boolean=?
box
box?
break
bytevector
bytevector-append
bytevector-copy
bytevector-copy!
bytevector-length
bytevector-u8-ref
bytevector-u8-set!
bytevector?
call-with-input-process
call-with-input-string
call-with-input-u8vector
call-with-input-vector
call-with-output-process
call-with-output-string
call-with-output-u8vector
call-with-output-vector
call-with-port
call/cc
cfun-conversion-exception-arguments
cfun-conversion-exception-code
cfun-conversion-exception-message
cfun-conversion-exception-procedure
cfun-conversion-exception?
char-foldcase
circular-list
clear-bit-field
close-port
command-line
compile-file
compile-file-to-target
condition-variable-broadcast!
condition-variable-name
condition-variable-signal!
condition-variable-specific
condition-variable-specific-set!
condition-variable?
configure-command-string
conjugate
cons*
console-port
continuation-capture
continuation-graft
continuation-return
continuation?
copy-bit-field
copy-file
cosh
cpu-time
create-directory
create-fifo
create-link
create-symbolic-link
create-temporary-directory
current-directory
current-error-port
current-exception-handler
current-jiffy
current-processor
current-readtable
current-second
current-thread
current-time
current-user-interrupt-handler
datum->syntax
datum-parsing-exception-kind
datum-parsing-exception-parameters
datum-parsing-exception-readenv
datum-parsing-exception?
deadlock-exception?
defer-user-interrupts
delete-directory
delete-file
delete-file-or-directory
digit-value
directory-files
display-continuation-backtrace
display-continuation-dynamic-environment
display-continuation-environment
display-dynamic-environment?
display-environment-set!
display-exception
display-exception-in-context
display-procedure-environment
divide-by-zero-exception-arguments
divide-by-zero-exception-procedure
divide-by-zero-exception?
drop
emergency-exit
eof-object
eq?-hash
equal?-hash
eqv?-hash
err-code->string
error
error-exception-message
error-exception-parameters
error-exception?
error-object-irritants
error-object-message
error-object?
exact
exact-integer?
exact-integer-sqrt
executable-path
exit
expression-parsing-exception-kind
expression-parsing-exception-parameters
expression-parsing-exception-source
expression-parsing-exception?
extract-bit-field
f32vector
f32vector->list
f32vector-append
f32vector-copy
f32vector-copy!
f32vector-fill!
f32vector-length
f32vector-ref
f32vector-set
f32vector-set!
f32vector-shrink!
f32vector?
f64vector
f64vector->list
f64vector-append
f64vector-copy
f64vector-copy!
f64vector-fill!
f64vector-length
f64vector-ref
f64vector-set
f64vector-set!
f64vector-shrink!
f64vector?
features
file-attributes
file-creation-time
file-device
file-error?
file-exists-exception-arguments
file-exists-exception-procedure
file-exists-exception?
file-exists?
file-group
file-info
file-info-attributes
file-info-creation-time
file-info-device
file-info-group
file-info-inode
file-info-last-access-time
file-info-last-change-time
file-info-last-modification-time
file-info-mode
file-info-number-of-links
file-info-owner
file-info-size
file-info-type
file-info?
file-inode
file-last-access-and-modification-times-set!
file-last-access-time
file-last-change-time
file-last-modification-time
file-mode
file-number-of-links
file-owner
file-size
file-type
finite?
first-bit-set
fixnum->flonum
fixnum-overflow-exception-arguments
fixnum-overflow-exception-procedure
fixnum-overflow-exception?
fixnum?
fl*
fl+
fl-
fl/
fl<
fl<=
fl=
fl>
fl>=
flabs
flacos
flacosh
flasin
flasinh
flatan
flatanh
flceiling
flcos
flcosh
fldenominator
fleven?
flexp
flexpm1
flexpt
flfinite?
flfloor
flhypot
flilogb
flinfinite?
flinteger?
fllog
fllog1p
flmax
flmin
flnan?
flnegative?
flnumerator
flodd?
flonum?
floor-quotient
floor-remainder
floor/
flpositive?
flround
flscalbn
flsin
flsinh
flsqrt
flsquare
fltan
fltanh
fltruncate
flush-output-port
flzero?
fold
fold-right
force-output
foreign-address
foreign-release!
foreign-released?
foreign-tags
foreign?
fx*
fx+
fx-
fx<
fx<=
fx=
fx>
fx>=
fxabs
fxand
fxarithmetic-shift
fxarithmetic-shift-left
fxarithmetic-shift-right
fxbit-count
fxbit-set?
fxeven?
fxfirst-bit-set
fxif
fxior
fxlength
fxmax
fxmin
fxmodulo
fxnegative?
fxnot
fxodd?
fxpositive?
fxquotient
fxremainder
fxsquare
fxwrap*
fxwrap+
fxwrap-
fxwrapabs
fxwraparithmetic-shift
fxwraparithmetic-shift-left
fxwraplogical-shift-right
fxwrapquotient
fxwrapsquare
fxxor
fxzero?
gc-report-set!
generate-proper-tail-calls
gensym
get-environment-variable
get-environment-variables
get-output-bytevector
get-output-string
get-output-u8vector
get-output-vector
getenv
group-info
group-info-gid
group-info-members
group-info-name
group-info?
heap-overflow-exception?
help
help-browser
host-info
host-info-addresses
host-info-aliases
host-info-name
host-info?
host-name
identity
inactive-thread-exception-arguments
inactive-thread-exception-procedure
inactive-thread-exception?
inexact
infinite?
initialized-thread-exception-arguments
initialized-thread-exception-procedure
initialized-thread-exception?
input-port-byte-position
input-port-bytes-buffered
input-port-char-position
input-port-characters-buffered
input-port-column
input-port-line
input-port-open?
input-port-readtable
input-port-readtable-set!
input-port-timeout-set!
integer-length
integer-nth-root
integer-sqrt
invalid-hash-number-exception-arguments
invalid-hash-number-exception-procedure
invalid-hash-number-exception?
invalid-utf8-encoding-exception-arguments
invalid-utf8-encoding-exception-procedure
invalid-utf8-encoding-exception?
iota
jiffies-per-second
join-timeout-exception-arguments
join-timeout-exception-procedure
join-timeout-exception?
keyword->string
keyword-expected-exception-arguments
keyword-expected-exception-procedure
keyword-expected-exception?
keyword-hash
keyword?
last
last-pair
length-mismatch-exception-arg-num
length-mismatch-exception-arguments
length-mismatch-exception-procedure
length-mismatch-exception?
link-flat
link-incremental
list->f32vector
list->f64vector
list->s16vector
list->s32vector
list->s64vector
list->s8vector
list->table
list->u16vector
list->u32vector
list->u64vector
list->u8vector
list-copy
list-set
list-set!
list-tabulate
mailbox-receive-timeout-exception-arguments
mailbox-receive-timeout-exception-procedure
mailbox-receive-timeout-exception?
main
make-bytevector
make-condition-variable
make-f32vector
make-f64vector
make-list
make-mutex
make-parameter
make-promise
make-random-source
make-root-thread
make-s16vector
make-s32vector
make-s64vector
make-s8vector
make-table
make-thread
make-thread-group
make-tls-context
make-u16vector
make-u32vector
make-u64vector
make-u8vector
make-will
module-not-found-exception-arguments
module-not-found-exception-procedure
module-not-found-exception?
multiple-c-return-exception?
mutex-lock!
mutex-name
mutex-specific
mutex-specific-set!
mutex-state
mutex-unlock!
mutex?
nan?
network-info
network-info-aliases
network-info-name
network-info-number
network-info?
no-such-file-or-directory-exception-arguments
no-such-file-or-directory-exception-procedure
no-such-file-or-directory-exception?
noncontinuable-exception-reason
noncontinuable-exception?
nonempty-input-port-character-buffer-exception-arguments
nonempty-input-port-character-buffer-exception-procedure
nonempty-input-port-character-buffer-exception?
nonprocedure-operator-exception-arguments
nonprocedure-operator-exception-code
nonprocedure-operator-exception-operator
nonprocedure-operator-exception-rte
nonprocedure-operator-exception?
number-of-arguments-limit-exception-arguments
number-of-arguments-limit-exception-procedure
number-of-arguments-limit-exception?
object->serial-number
object->string
object->u8vector
open-binary-input-file
open-binary-output-file
open-directory
open-dummy
open-event-queue
open-file
open-input-bytevector
open-input-process
open-input-string
open-input-u8vector
open-input-vector
open-output-bytevector
open-output-process
open-output-string
open-output-u8vector
open-output-vector
open-process
open-string
open-string-pipe
open-tcp-client
open-tcp-server
open-u8vector
open-u8vector-pipe
open-udp
open-vector
open-vector-pipe
os-exception-arguments
os-exception-code
os-exception-message
os-exception-procedure
os-exception?
output-port-byte-position
output-port-char-position
output-port-column
output-port-line
output-port-open?
output-port-readtable
output-port-readtable-set!
output-port-timeout-set!
output-port-width
path-directory
path-expand
path-extension
path-normalize
path-strip-directory
path-strip-extension
path-strip-trailing-directory-separator
path-strip-volume
path-volume
port-io-exception-handler-set!
port-settings-set!
port?
pp
pretty-print
primordial-exception-handler
print
println
process-pid
process-status
process-times
processor-id
processor?
promise?
protocol-info
protocol-info-aliases
protocol-info-name
protocol-info-number
protocol-info?
raise
random-f64vector
random-integer
random-real
random-source-make-f64vectors
random-source-make-integers
random-source-make-reals
random-source-make-u8vectors
random-source-pseudo-randomize!
random-source-randomize!
random-source-state-ref
random-source-state-set!
random-source?
random-u8vector
range-exception-arg-num
range-exception-arguments
range-exception-procedure
range-exception?
read-all
read-bytevector
read-bytevector!
read-error?
read-line
read-string
read-substring
read-subu8vector
read-u8
readtable-case-conversion?
readtable-case-conversion?-set
readtable-comment-handler
readtable-comment-handler-set
readtable-eval-allowed?
readtable-eval-allowed?-set
readtable-keywords-allowed?
readtable-keywords-allowed?-set
readtable-max-unescaped-char
readtable-max-unescaped-char-set
readtable-max-write-length
readtable-max-write-length-set
readtable-max-write-level
readtable-max-write-level-set
readtable-sharing-allowed?
readtable-sharing-allowed?-set
readtable-start-syntax
readtable-start-syntax-set
readtable-write-cdr-read-macros?
readtable-write-cdr-read-macros?-set
readtable-write-extended-read-macros?
readtable-write-extended-read-macros?-set
readtable?
real-time
rename-file
repl-display-environment?
repl-error-port
repl-input-port
repl-output-port
repl-result-history-max-length-set!
repl-result-history-ref
replace-bit-field
reverse!
rpc-remote-error-exception-arguments
rpc-remote-error-exception-message
rpc-remote-error-exception-procedure
rpc-remote-error-exception?
s16vector
s16vector->list
s16vector-append
s16vector-copy
s16vector-copy!
s16vector-fill!
s16vector-length
s16vector-ref
s16vector-set
s16vector-set!
s16vector-shrink!
s16vector?
s32vector
s32vector->list
s32vector-append
s32vector-copy
s32vector-copy!
s32vector-fill!
s32vector-length
s32vector-ref
s32vector-set
s32vector-set!
s32vector-shrink!
s32vector?
s64vector
s64vector->list
s64vector-append
s64vector-copy
s64vector-copy!
s64vector-fill!
s64vector-length
s64vector-ref
s64vector-set
s64vector-set!
s64vector-shrink!
s64vector?
s8vector
s8vector->list
s8vector-append
s8vector-copy
s8vector-copy!
s8vector-fill!
s8vector-length
s8vector-ref
s8vector-set
s8vector-set!
s8vector-shrink!
s8vector?
scheduler-exception-reason
scheduler-exception?
seconds->time
serial-number->object
service-info
service-info-aliases
service-info-name
service-info-port-number
service-info-protocol
service-info?
set-box!
setenv
sfun-conversion-exception-arguments
sfun-conversion-exception-code
sfun-conversion-exception-message
sfun-conversion-exception-procedure
sfun-conversion-exception?
shell-command
sinh
six.!
six.!x
six.&x
six.*x
six.++x
six.+x
six.--x
six.-x
six.arrow
six.break
six.call
six.case
six.clause
six.compound
six.cons
six.continue
six.define-procedure
six.define-variable
six.do-while
six.dot
six.for
six.goto
six.identifier
six.if
six.index
six.label
six.list
six.literal
six.make-array
six.new
six.null
six.prefix
six.procedure
six.procedure-body
six.return
six.switch
six.while
six.x!=y
six.x%=y
six.x%y
six.x&&y
six.x&=y
six.x&y
six.x*=y
six.x*y
six.x++
six.x+=y
six.x+y
|six.x,y|
six.x--
six.x-=y
six.x-y
six.x/=y
six.x/y
six.x:-y
six.x:=y
six.x:y
six.x<<=y
six.x<<y
six.x<=y
six.x<y
six.x==y
six.x=y
six.x>=y
six.x>>=y
six.x>>y
six.x>y
six.x?y:z
six.x^=y
six.x^y
|six.x\|=y|
|six.x\|y|
|six.x\|\|y|
six.~x
socket-info-address
socket-info-family
socket-info-port-number
socket-info?
square
stack-overflow-exception?
started-thread-exception-arguments
started-thread-exception-procedure
started-thread-exception?
step
step-level-set!
string->keyword
string->uninterned-keyword
string->uninterned-symbol
string->utf8
string->vector
string-ci=?-hash
string-copy!
string-downcase
string-foldcase
string-for-each
string-map
string-set
string-shrink!
string-upcase
string=?-hash
subf32vector
subf32vector-fill!
subf32vector-move!
subf64vector
subf64vector-fill!
subf64vector-move!
subs16vector
subs16vector-fill!
subs16vector-move!
subs32vector
subs32vector-fill!
subs32vector-move!
subs64vector
subs64vector-fill!
subs64vector-move!
subs8vector
subs8vector-fill!
subs8vector-move!
substring-fill!
substring-move!
subu16vector
subu16vector-fill!
subu16vector-move!
subu32vector
subu32vector-fill!
subu32vector-move!
subu64vector
subu64vector-fill!
subu64vector-move!
subu8vector
subu8vector-fill!
subu8vector-move!
subvector
subvector-fill!
subvector-move!
symbol-hash
symbol=?
syntax->datum
syntax->list
syntax->vector
system-stamp
system-type
system-type-string
system-version
system-version-string
table->list
table-copy
table-for-each
table-length
table-merge
table-merge!
table-ref
table-search
table-set!
table?
take
tanh
tcp-client-local-socket-info
tcp-client-peer-socket-info
tcp-client-self-socket-info
tcp-server-socket-info
tcp-service-register!
tcp-service-unregister!
terminated-thread-exception-arguments
terminated-thread-exception-procedure
terminated-thread-exception?
test-bit-field?
textual-port?
thread-base-priority
thread-base-priority-set!
thread-group->thread-group-list
thread-group->thread-group-vector
thread-group->thread-list
thread-group->thread-vector
thread-group-name
thread-group-parent
thread-group-resume!
thread-group-specific
thread-group-specific-set!
thread-group-suspend!
thread-group-terminate!
thread-group?
thread-init!
thread-interrupt!
thread-join!
thread-mailbox-extract-and-rewind
thread-mailbox-next
thread-mailbox-rewind
thread-name
thread-priority-boost
thread-priority-boost-set!
thread-quantum
thread-quantum-set!
thread-receive
thread-resume!
thread-send
thread-sleep!
thread-specific
thread-specific-set!
thread-start!
thread-state
thread-state-abnormally-terminated-reason
thread-state-abnormally-terminated?
thread-state-initialized?
thread-state-normally-terminated-result
thread-state-normally-terminated?
thread-state-running-processor
thread-state-running?
thread-state-uninitialized?
thread-state-waiting-for
thread-state-waiting-timeout
thread-state-waiting?
thread-suspend!
thread-terminate!
thread-thread-group
thread-yield!
thread?
time->seconds
time?
timeout->time
top
touch
trace
truncate-quotient
truncate-remainder
truncate/
tty-history
tty-history-max-length-set!
tty-history-set!
tty-mode-set!
tty-paren-balance-duration-set!
tty-text-attributes-set!
tty-type-set!
tty?
type-exception-arg-num
type-exception-arguments
type-exception-procedure
type-exception-type-id
type-exception?
u16vector
u16vector->list
u16vector-append
u16vector-copy
u16vector-copy!
u16vector-fill!
u16vector-length
u16vector-ref
u16vector-set
u16vector-set!
u16vector-shrink!
u16vector?
u32vector
u32vector->list
u32vector-append
u32vector-copy
u32vector-copy!
u32vector-fill!
u32vector-length
u32vector-ref
u32vector-set
u32vector-set!
u32vector-shrink!
u32vector?
u64vector
u64vector->list
u64vector-append
u64vector-copy
u64vector-copy!
u64vector-fill!
u64vector-length
u64vector-ref
u64vector-set
u64vector-set!
u64vector-shrink!
u64vector?
u8-ready?
u8vector
u8vector->list
u8vector->object
u8vector-append
u8vector-copy
u8vector-copy!
u8vector-fill!
u8vector-length
u8vector-ref
u8vector-set
u8vector-set!
u8vector-shrink!
u8vector?
udp-destination-set!
udp-local-socket-info
udp-read-subu8vector
udp-read-u8vector
udp-source-socket-info
udp-write-subu8vector
udp-write-u8vector
unbound-global-exception-code
unbound-global-exception-rte
unbound-global-exception-variable
unbound-global-exception?
unbound-os-environment-variable-exception-arguments
unbound-os-environment-variable-exception-procedure
unbound-os-environment-variable-exception?
unbound-serial-number-exception-arguments
unbound-serial-number-exception-procedure
unbound-serial-number-exception?
unbound-key-exception-arguments
unbound-key-exception-procedure
unbound-key-exception?
unbox
unbreak
uncaught-exception-arguments
uncaught-exception-procedure
uncaught-exception-reason
uncaught-exception?
uninitialized-thread-exception-arguments
uninitialized-thread-exception-procedure
uninitialized-thread-exception?
uninterned-keyword?
uninterned-symbol?
unknown-keyword-argument-exception-arguments
unknown-keyword-argument-exception-procedure
unknown-keyword-argument-exception?
unterminated-process-exception-arguments
unterminated-process-exception-procedure
unterminated-process-exception?
untrace
user-info
user-info-gid
user-info-home
user-info-name
user-info-shell
user-info-uid
user-info?
user-name
utf8->string
vector->string
vector-append
vector-cas!
vector-copy
vector-copy!
vector-for-each
vector-inc!
vector-map
vector-set
vector-shrink!
void
will-execute!
will-testator
will?
with-exception-catcher
with-exception-handler
with-input-from-port
with-input-from-process
with-input-from-string
with-input-from-u8vector
with-input-from-vector
with-output-to-port
with-output-to-process
with-output-to-string
with-output-to-u8vector
with-output-to-vector
write-bytevector
write-shared
write-simple
write-string
write-substring
write-subu8vector
write-u8
wrong-number-of-arguments-exception-arguments
wrong-number-of-arguments-exception-procedure
wrong-number-of-arguments-exception?
wrong-number-of-values-exception-code
wrong-number-of-values-exception-rte
wrong-number-of-values-exception-vals
wrong-number-of-values-exception?
wrong-processor-c-return-exception?
xcons

))

;;;============================================================================
