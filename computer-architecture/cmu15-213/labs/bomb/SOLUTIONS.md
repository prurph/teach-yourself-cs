# Bomb Lab Solutions

Some notes on how I solved this.

## General Flow

1. Set a break on `explode_bomb` to avoid mishaps
2. Set a break on the phase to investigate `phase_1`, etc
3. Run the code until the desired phase
4. Use `disassemble` to see what's going on, and set additional breakpoints

## Phase 1

```asm
Breakpoint 5, 0x0000000000400ee0 in phase_1 ()
(gdb) disassemble
Dump of assembler code for function phase_1:
=> 0x0000000000400ee0 <+0>:     sub    $0x8,%rsp
   0x0000000000400ee4 <+4>:     mov    $0x402400,%esi
   0x0000000000400ee9 <+9>:     call   0x401338 <strings_not_equal>
   0x0000000000400eee <+14>:    test   %eax,%eax
   0x0000000000400ef0 <+16>:    je     0x400ef7 <phase_1+23>
   0x0000000000400ef2 <+18>:    call   0x40143a <explode_bomb>
   0x0000000000400ef7 <+23>:    add    $0x8,%rsp
   0x0000000000400efb <+27>:    ret
End of assembler dump.
```

Relevant lines are:

```asm
mov    $0x402400,%esi                # load something into the second argument
call   0x401338 <strings_not_equal>  # call this `strings_not_equal` function
test   %eax,%eax                     # test bitwise &'s the return value
je     0x400ef7 <phase_1+23>         # keep going with phase_1 if test was 0
call   0x40143a <explode_bomb>       # otherwise explode
```

The `test %reg,%reg` followed by `je` is a common pattern to jump if `%reg` is zero

- `test` does bitwise and, setting flags based on the result
- `je` jumps if the zero flag is set, meaning `%reg` was zero

Overall therefore, it seems like our input must match a string, if it doesn't, `strings_not_equal` has a non-zero return, we do not jump, and instead call `explode_bomb`

- In fact stepping through with `si` to the test line we find `%eax` is our original input string if it doesn't match
- We also see with `x $rdi` that the first argument is our original input string
- It stands to reason whatever is in `%esi` (second argument) is the desired string:

```asm
(gdb) x/s $esi
0x402400:       "Border relations with Canada have never been better."
```

### Answer
 
`Border relations with Canada have never been better.`

## Phase 2

From a similar analysis, we find a function `read_six_numbers` is called in `phase_2`, so set a break there:

```asm
Dump of assembler code for function read_six_numbers:
=> 0x000000000040145c <+0>:     sub    $0x18,%rsp
   0x0000000000401460 <+4>:     mov    %rsi,%rdx
   0x0000000000401463 <+7>:     lea    0x4(%rsi),%rcx
   0x0000000000401467 <+11>:    lea    0x14(%rsi),%rax
   0x000000000040146b <+15>:    mov    %rax,0x8(%rsp)
   0x0000000000401470 <+20>:    lea    0x10(%rsi),%rax
   0x0000000000401474 <+24>:    mov    %rax,(%rsp)
   0x0000000000401478 <+28>:    lea    0xc(%rsi),%r9
   0x000000000040147c <+32>:    lea    0x8(%rsi),%r8
   0x0000000000401480 <+36>:    mov    $0x4025c3,%esi
   0x0000000000401485 <+41>:    mov    $0x0,%eax
   0x000000000040148a <+46>:    call   0x400bf0 <__isoc99_sscanf@plt>
   0x000000000040148f <+51>:    cmp    $0x5,%eax
   0x0000000000401492 <+54>:    jg     0x401499 <read_six_numbers+61>
   0x0000000000401494 <+56>:    call   0x40143a <explode_bomb>
   0x0000000000401499 <+61>:    add    $0x18,%rsp
   0x000000000040149d <+65>:    ret
```

Some interesting lines are:

```asm
mov    $0x4025c3,%esi                  # load something into second argument
mov    $0x0,%eax
call   0x400bf0 <__isoc99_sscanf@plt>  # call sscanf
cmp    $0x5,%eax                       # compare retval to 5
jg     0x401499 <read_six_numbers+61>  # if return > 5, don't explode
call   0x40143a <explode_bomb>
```

[sscanf](https://www.educative.io/edpresso/how-to-read-data-using-sscanf-in-c) or `man sscanf` reads formatted data. Its arguments are:

1. A pointer to a string to be read
2. A format string to compare against
3. Remaining arguments to store the matched value(s)

It returns the number of matched values, so we expect that our input string needs to be six numbers, all of which match, therefore making `%eax == 6`, thereby setting the greater than when `cmp    $0x5,%eax` and jumping before exploding.

- In fact we can see the format string by stepping up to the `sscanf` call and `x/s %rsi` (use `stepi` to step assembly instructions)
- Doing so shows it is `"%d %d %d %d %d %d"`

It gets more complex though, because the numeric values are actually being used.

- Digging into the manipulations shows it's looking for 1 2 4 8 16 32
- In truth I got lucky here and realized it was 1 2 4 and then guessed the rest

### Answer

1 2 4 8 16 32

## Phase 3

### Aside: Printing Jump Tables

Jump tables will appear like `jmp    *0x402470(,%rax,8)` with the `*`

- Use `p/a *0x402470@x` to print `x` elements. These will have symbols to which they point!
- Can also do `x/[count]xg *0x402470` to see this

### Solution Method

Overall, this is another `sscanf` call with a `%d %d` format string.

- Before the `sscanf` there are two `lea` calls
    - Set `%rdx` (third argument) to be `0xc(%rsp)`
    - Set `%rcx` (fourth argument) to be `0x8(%rsp)`
- The combined effect is that the first number we entered ends up in memory at `%rsp + 0xC`, and the second in memory at `%rsp + 0x8`
- Subsequent jump table lookup based on the first argument
- Each case of the switch sets the return value to some literal
- The final check is that return value against the second number provided, which is stored in `0x8(%rsp)`
- Thus there needs to be an alignment between where you jump (first number) and what value is set in that case (second number)

Annotated assembly

```asm
Dump of assembler code for function phase_3:
   0x0000000000400f43 <+0>:     sub    $0x18,%rsp

   # Set %rcx to %rsp + 0xC. This means the fourth argument
   # of our `sscanf` call will point to %rsp + 0xC, which means
   # the second number we pass in has its value stored at that address!
   0x0000000000400f47 <+4>:     lea    0xc(%rsp),%rcx

   # Arg three points to %rsp + 8, the value of our first number will get put there
   0x0000000000400f4c <+9>:     lea    0x8(%rsp),%rdx
   0x0000000000400f51 <+14>:    mov    $0x4025cf,%esi
=> 0x0000000000400f56 <+19>:    mov    $0x0,%eax
   0x0000000000400f5b <+24>:    call   0x400bf0 <__isoc99_sscanf@plt>

   # Need to have parsed two numbers
   0x0000000000400f60 <+29>:    cmp    $0x1,%eax

   0x0000000000400f63 <+32>:    jg     0x400f6a <phase_3+39>
   0x0000000000400f65 <+34>:    call   0x40143a <explode_bomb>

   # Remember 0x8(%rsp) holds the value of our first argument: it
   # must be less than 7 of we jump to +106 explode bomb next line
   0x0000000000400f6a <+39>:    cmpl   $0x7,0x8(%rsp)
   0x0000000000400f6f <+44>:    ja     0x400fad <phase_3+106>

   # Stick %rsp + 8 into %eax. It's going to be used for our jump table lookup
   0x0000000000400f71 <+46>:    mov    0x8(%rsp),%eax

   # Jump based on %rax, which we just set to %rsp+8 on +46 
   0x0000000000400f75 <+50>:    jmp    *0x402470(,%rax,8)

   # In one solution, we pass in 0 207. 0 happens to jump us to +57 below, where
   # the return value is set to 0xCF = 207.
   0x0000000000400f7c <+57>:    mov    $0xcf,%eax
   0x0000000000400f81 <+62>:    jmp    0x400fbe <phase_3+123>
   0x0000000000400f83 <+64>:    mov    $0x2c3,%eax
   0x0000000000400f88 <+69>:    jmp    0x400fbe <phase_3+123>
   0x0000000000400f8a <+71>:    mov    $0x100,%eax
   0x0000000000400f8f <+76>:    jmp    0x400fbe <phase_3+123>
   0x0000000000400f91 <+78>:    mov    $0x185,%eax
   0x0000000000400f96 <+83>:    jmp    0x400fbe <phase_3+123>
   0x0000000000400f98 <+85>:    mov    $0xce,%eax
   0x0000000000400f9d <+90>:    jmp    0x400fbe <phase_3+123>
   0x0000000000400f9f <+92>:    mov    $0x2aa,%eax
   0x0000000000400fa4 <+97>:    jmp    0x400fbe <phase_3+123>
   0x0000000000400fa6 <+99>:    mov    $0x147,%eax
   0x0000000000400fab <+104>:   jmp    0x400fbe <phase_3+123>
   0x0000000000400fad <+106>:   call   0x40143a <explode_bomb>
   0x0000000000400fb2 <+111>:   mov    $0x0,%eax
   0x0000000000400fb7 <+116>:   jmp    0x400fbe <phase_3+123>
   0x0000000000400fb9 <+118>:   mov    $0x137,%eax

   # This is the final cmp we want: %eax must equal %rsp + 0xC; recall this
   # is our second argument.
   # Also note we have +123 here which mimics all of the jumps above: +123
   # is the function resuming after the switch.
   0x0000000000400fbe <+123>:   cmp    0xc(%rsp),%eax
   0x0000000000400fc2 <+127>:   je     0x400fc9 <phase_3+134>
   0x0000000000400fc4 <+129>:   call   0x40143a <explode_bomb>
   0x0000000000400fc9 <+134>:   add    $0x18,%rsp
   0x0000000000400fcd <+138>:   ret
```

### Answer

`0 207`

## Phase 4

Again, same setup of `sscanf` to parse two numbers and set them in offsets from `%rsp`. This progresses into:

```asm
   0x000000000040103a <+46>:    mov    $0xe,%edx
   0x000000000040103f <+51>:    mov    $0x0,%esi
   0x0000000000401044 <+56>:    mov    0x8(%rsp),%edi
=> 0x0000000000401048 <+60>:    call   0x400fce <func4>
```

We're setting `%edx` to 14, putting our first input number into `%edi` and calling `func4`, which does the following.

- There's a lot of obfuscating math going on, but the keys are two comparisons
    - On `func4+20/+22`: `%ecx <= %edi` to jump, otherwise loop back to the top
    - On `func4+41/+43`: `%ecx >= %edi` to jump, otherwise loop
- Note that `%edi` is the first input number, and it _doesn't change_
- If we do the math for the top loop part we discover that at the point of the first comparison, `%ecx == 7`
- Therefore `%edi`, the first input number, must be exactly 7

```asm
Dump of assembler code for function func4:
   0x0000000000400fce <+0>:     sub    $0x8,%rsp
=> 0x0000000000400fd2 <+4>:     mov    %edx,%eax
   0x0000000000400fd4 <+6>:     sub    %esi,%eax
   0x0000000000400fd6 <+8>:     mov    %eax,%ecx
   0x0000000000400fd8 <+10>:    shr    $0x1f,%ecx
   0x0000000000400fdb <+13>:    add    %ecx,%eax
   0x0000000000400fdd <+15>:    sar    %eax
   0x0000000000400fdf <+17>:    lea    (%rax,%rsi,1),%ecx

   # %edi is first input number, %ecx is 7 the first time through
   # %ecx <= %edi
   0x0000000000400fe2 <+20>:    cmp    %edi,%ecx
   0x0000000000400fe4 <+22>:    jle    0x400ff2 <func4+36>

   0x0000000000400fe6 <+24>:    lea    -0x1(%rcx),%edx
   0x0000000000400fe9 <+27>:    call   0x400fce <func4>
   0x0000000000400fee <+32>:    add    %eax,%eax
   0x0000000000400ff0 <+34>:    jmp    0x401007 <func4+57>
   0x0000000000400ff2 <+36>:    mov    $0x0,%eax

   # %edi does not change, and here we need %ecx >= %edi
   # In concert with +20/+22 this means %edi = %ecx = 7
   0x0000000000400ff7 <+41>:    cmp    %edi,%ecx
   0x0000000000400ff9 <+43>:    jge    0x401007 <func4+57>
   0x0000000000400ffb <+45>:    lea    0x1(%rcx),%esi
   0x0000000000400ffe <+48>:    call   0x400fce <func4>
   0x0000000000401003 <+53>:    lea    0x1(%rax,%rax,1),%eax
   0x0000000000401007 <+57>:    add    $0x8,%rsp
   0x000000000040100b <+61>:    ret
 ```

With these comparisons satisfied, `func4` returns and we're back in `phase_4`. The relevant lines are:

```asm
   # This just tests the output of %eax from func4: 1 means passing
=> 0x000000000040104d <+65>:    test   %eax,%eax
   0x000000000040104f <+67>:    jne    0x401058 <phase_4+76>
   
   # This is the relevant compasrison: %rsp + 0xC is our second input number;
   # it must equal 0 to pass.
   0x0000000000401051 <+69>:    cmpl   $0x0,0xc(%rsp)
   0x0000000000401056 <+74>:    je     0x40105d <phase_4+81>
   0x0000000000401058 <+76>:    call   0x40143a <explode_bomb>
   0x000000000040105d <+81>:    add    $0x18,%rsp
   0x0000000000401061 <+85>:    ret
```

This gives us our final answer of `7 0`

### Answer

`7 0`

## Phase 5

### Hack/Experimental Solution

- I noticed a call to `strings_not_equal` where `$rsi` always pointed at the string "flyers", and `$rdi` was a permutation of my string
- There is a check for string length of 6, so I went through submitting "abcdef", "hijklm", etc., and checking what the translation was going into the `strings_not_equal` call
- It turns out each letter is mapped to something else (doesn't seem to be a Caesar cipher though)
- By learning the mappings I was able to submit something that translated to the desired "flyers"

### Deeper Understanding

The key is here from +41 to +74, and noticing that there's this random `movzbl 0x4024b0(%rdx),%edx` on +55. That literal value is an array containing the translation string.

```asm
   # %rax is 0, %rbx is our input string
   # This is taking the first byte of %rbx + 0*1 = %rbx, putting it in
   # %ecx (this is the first character of our input!)
   # If you p/c $ecx at this point you'll see your character and it's
   # ASCII code
=> 0x000000000040108b <+41>:    movzbl (%rbx,%rax,1),%ecx

   # %cl is actually just the low byte of %rcx/%ecx, so these
   # instructions are moving it to %rdx and masking its first bit (+52)
   # to zero out the rest of %rdx
   0x000000000040108f <+45>:    mov    %cl,(%rsp)
   0x0000000000401092 <+48>:    mov    (%rsp),%rdx
   0x0000000000401096 <+52>:    and    $0xf,%edx

   # This is the key: %rdx is set as the first four bits (from the movzbl
   # plus the & with 0xF) of our input character. It's now being used to
   # index into 0x4024b0. If we look at that:
   # (gdb) x/8s 0x4024b0
   # 0x4024b0 <array.3449>:  "maduiersnfotvbylSo you think you can stop the bomb with ctrl-c, do you?"
   # 0x4024f8:       "Curses, you've found the secret phase!"
   # 0x40251f:       ""
   # 0x402520:       "But finding it and solving it are quite different..."
   # 0x402555:       ""
   # 0x402556:       ""
   # 0x402557:       ""
   # 0x402558:       "Congratulations! You've defused the bomb!"
   # We see the first entry has a bunch of letters, so the offset is
   # basically looking up a translation, and we just need to find what
   # translates to "flyers".
   # Example:
   # input string i = 105 = 1101001
   # mask first byte = 1001 =  0x9
   # 0x4024b0 + 9 * 1 = 10th character, or f
   0x0000000000401099 <+55>:    movzbl 0x4024b0(%rdx),%edx
   0x00000000004010a0 <+62>:    mov    %dl,0x10(%rsp,%rax,1)

   # Increment %rax
   0x00000000004010a4 <+66>:    add    $0x1,%rax

   # If %rax isn't yet 6, keep looping back to +41. So we're running
   # the loop for each character
   0x00000000004010a8 <+70>:    cmp    $0x6,%rax
   0x00000000004010ac <+74>:    jne    0x40108b <phase_5+41>
   0x00000000004010ae <+76>:    movb   $0x0,0x16(%rsp)
   0x00000000004010b3 <+81>:    mov    $0x40245e,%esi
   0x00000000004010b8 <+86>:    lea    0x10(%rsp),%rdi
   0x00000000004010bd <+91>:    call   0x401338 <strings_not_equal>
```

### Answer

`ionevw`

## Phase 6

```asm
0x00000000004010f4 <+0>:     push   %r14
0x00000000004010f6 <+2>:     push   %r13
0x00000000004010f8 <+4>:     push   %r12
0x00000000004010fa <+6>:     push   %rbp
0x00000000004010fb <+7>:     push   %rbx
0x00000000004010fc <+8>:     sub    $0x50,%rsp
# Point %r13 at the stack
0x0000000000401100 <+12>:    mov    %rsp,%r13
0x0000000000401103 <+15>:    mov    %rsp,%rsi

# Again parse 6 numbers
0x0000000000401106 <+18>:    call   0x40145c <read_six_numbers>
0x000000000040110b <+23>:    mov    %rsp,%r14
0x000000000040110e <+26>:    mov    $0x0,%r12d

0x0000000000401114 <+32>:    mov    %r13,%rbp

# Dereference %r13, which is pointing at the stack 
# This loads our first input into %eax
# %r13 will be incremented on +89 so we'll move through
# each input in turn.
0x0000000000401117 <+35>:    mov    0x0(%r13),%eax
# That input must be less than or equal to 6 (subtract 1 and compare to 5)
0x000000000040111b <+39>:    sub    $0x1,%eax
0x000000000040111e <+42>:    cmp    $0x5,%eax
0x0000000000401121 <+45>:    jbe    0x401128 <phase_6+52>
0x0000000000401123 <+47>:    call   0x40143a <explode_bomb>

# %r12d is the loop counter, increment and exit this loop
# to +95 when we've analyzed 6 entries. It will start at 1.
0x0000000000401128 <+52>:    add    $0x1,%r12d
0x000000000040112c <+56>:    cmp    $0x6,%r12d
0x0000000000401130 <+60>:    je     0x401153 <phase_6+95>

# Set %rax to the loop counter
0x0000000000401132 <+62>:    mov    %r12d,%ebx
0x0000000000401135 <+65>:    movslq %ebx,%rax

# Index into the stack by the loop counter and put it in %eax
0x0000000000401138 <+68>:    mov    (%rsp,%rax,4),%eax

# Remember %rbp was our original entry, so here
# we check that each entry does not equal the first and is
# less than or equal to 6 (+81/+84).
0x000000000040113b <+71>:    cmp    %eax,0x0(%rbp)
0x000000000040113e <+74>:    jne    0x401145 <phase_6+81>
0x0000000000401140 <+76>:    call   0x40143a <explode_bomb>
0x0000000000401145 <+81>:    add    $0x1,%ebx
0x0000000000401148 <+84>:    cmp    $0x5,%ebx
0x000000000040114b <+87>:    jle    0x401135 <phase_6+65>

# Add 4 to %r13 and go to +32. This will then compare entry
# 3 to 2, and so forth. The overall effect is that we must
# have input 6 numbers, all below 7, and all distinct, such 
# as 1 2 3 4 5 6. When we're all done +56 jumps to +95.
0x000000000040114d <+89>:    add    $0x4,%r13
0x0000000000401151 <+93>:    jmp    0x401114 <phase_6+32>

# %rsp and %r14 both point to our input
# Set %rsi to %rsp + 0x18 = %rsp + 24; this is
# marking the end of our inputs:
#   6 * 4-byte words = 24 bytes
# The comparison on +121 means we loop back to +108
# until we get there
0x0000000000401153 <+95>:    lea    0x18(%rsp),%rsi
0x0000000000401158 <+100>:   mov    %r14,%rax

# Load 7 into %ecx then %edx
0x000000000040115b <+103>:   mov    $0x7,%ecx
0x0000000000401160 <+108>:   mov    %ecx,%edx

# Subtract the value pointed at by %rax (our input) from 7...
0x0000000000401162 <+110>:   sub    (%rax),%edx
# ...and store it back at that address (i.e. inputs[i] = 7 - inputs[i])
0x0000000000401164 <+112>:   mov    %edx,(%rax)

# Move to our next input (adding 4 because these are 32-bit words)
0x0000000000401166 <+114>:   add    $0x4,%rax

# Check to see if we're done yet (%rsi is the end as above)
0x000000000040116a <+118>:   cmp    %rsi,%rax
0x000000000040116d <+121>:   jne    0x401160 <phase_6+108>

# When we are done with the above loop we have effectively done:
# my_6_numbers.map(n => 7 - n)
0x000000000040116f <+123>:   mov    $0x0,%esi
0x0000000000401174 <+128>:   jmp    0x401197 <phase_6+163>

# This section indexes into a linked-list data structure
# at $0x6032d0, which is stored in %edx. It looks like:
#  (gdb) x/24wx 0x6032d0
#
#  0x6032d0 <node1>:       0x0000014c      0x00000001      0x006032e0      0x00000000
#  0x6032e0 <node2>:       0x000000a8      0x00000002      0x006032f0      0x00000000
#  0x6032f0 <node3>:       0x0000039c      0x00000003      0x00603300      0x00000000
#  0x603300 <node4>:       0x000002b3      0x00000004      0x00603310      0x00000000
#  0x603310 <node5>:       0x000001dd      0x00000005      0x00603320      0x00000000
#  0x603320 <node6>:       0x000001bb      0x00000006      0x00000000      0x00000000
#
# Where the first word (column) is the node's value, the second is our input, and
# the third is a pointer to the next node (note how its value references the next
# memory address). This sequence takes our inputs (that have been mapped to 7-x)
# in order, and finds the matching value from the 2nd column, then links it to
# the next node by the next match in our inputs. Finally we walk the nodes and
# ensure they are in descending order (+222/+243) based on the first column of the
# nodes. Thus we need our nodes to connect:
#
#   node3 0x39c > node4 0x2b3 > node5 0x1dd > node6 0x1bb > node1 0x14c > node2 0xa8
#
# Taking into account the 7 - x mapping this means the sequence is 7 - inputs[0] = 3,
# 7 - inputs[1] = 4, etc., so overall: 4 3 2 1 6 5
0x0000000000401176 <+130>:   mov    0x8(%rdx),%rdx
0x000000000040117a <+134>:   add    $0x1,%eax
0x000000000040117d <+137>:   cmp    %ecx,%eax
0x000000000040117f <+139>:   jne    0x401176 <phase_6+130>
0x0000000000401181 <+141>:   jmp    0x401188 <phase_6+148>
0x0000000000401183 <+143>:   mov    $0x6032d0,%edx
0x0000000000401188 <+148>:   mov    %rdx,0x20(%rsp,%rsi,2)
0x000000000040118d <+153>:   add    $0x4,%rsi
0x0000000000401191 <+157>:   cmp    $0x18,%rsi
0x0000000000401195 <+161>:   je     0x4011ab <phase_6+183>
0x0000000000401197 <+163>:   mov    (%rsp,%rsi,1),%ecx
0x000000000040119a <+166>:   cmp    $0x1,%ecx
0x000000000040119d <+169>:   jle    0x401183 <phase_6+143>
0x000000000040119f <+171>:   mov    $0x1,%eax
0x00000000004011a4 <+176>:   mov    $0x6032d0,%edx
0x00000000004011a9 <+181>:   jmp    0x401176 <phase_6+130>
0x00000000004011ab <+183>:   mov    0x20(%rsp),%rbx
0x00000000004011b0 <+188>:   lea    0x28(%rsp),%rax
0x00000000004011b5 <+193>:   lea    0x50(%rsp),%rsi
0x00000000004011ba <+198>:   mov    %rbx,%rcx
0x00000000004011bd <+201>:   mov    (%rax),%rdx
0x00000000004011c0 <+204>:   mov    %rdx,0x8(%rcx)
0x00000000004011c4 <+208>:   add    $0x8,%rax
0x00000000004011c8 <+212>:   cmp    %rsi,%rax
0x00000000004011cb <+215>:   je     0x4011d2 <phase_6+222>
0x00000000004011cd <+217>:   mov    %rdx,%rcx
0x00000000004011d0 <+220>:   jmp    0x4011bd <phase_6+201>
0x00000000004011d2 <+222>:   movq   $0x0,0x8(%rdx)
# Set the loop counter to 5
0x00000000004011da <+230>:   mov    $0x5,%ebp

# %rbx is the current node, set %rax to the address
# of the next (+8 from two 4-byte words; the address
# is the third word of the node "struct").
0x00000000004011df <+235>:   mov    0x8(%rbx),%rax
# Set the %eax to the value (first word) of the next node
0x00000000004011e3 <+239>:   mov    (%rax),%eax
# Compare prior node value <> next node value
0x00000000004011e5 <+241>:   cmp    %eax,(%rbx)
# Prior node value must be greater than or equal to this one 
0x00000000004011e7 <+243>:   jge    0x4011ee <phase_6+250>
0x00000000004011e9 <+245>:   call   0x40143a <explode_bomb>

# Move to the next node and repeat
0x00000000004011ee <+250>:   mov    0x8(%rbx),%rbx
# Decrement loop counter
0x00000000004011f2 <+254>:   sub    $0x1,%ebp
# jne tests if zero flag is set, so this is ebp != 0
0x00000000004011f5 <+257>:   jne    0x4011df <phase_6+235>
0x00000000004011f7 <+259>:   add    $0x50,%rsp
0x00000000004011fb <+263>:   pop    %rbx
0x00000000004011fc <+264>:   pop    %rbp
0x00000000004011fd <+265>:   pop    %r12
0x00000000004011ff <+267>:   pop    %r13
0x0000000000401201 <+269>:   pop    %r14
0x0000000000401203 <+271>:   ret
```

### Answer
`4 3 2 1 6 5`
