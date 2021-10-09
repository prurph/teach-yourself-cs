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