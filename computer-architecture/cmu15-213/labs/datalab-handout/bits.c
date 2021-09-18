/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
//1
/* 
 * bitXor - x^y using only ~ and & 
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y) {
  /* a) x ^ y = (x & ~y) | (~x & y)     xor is x and not y, or y and not x
   * b) x | y = ~(~x & ~y)              x or y is not (not x and not y)
   * c) x ^ y = ~(~(x & ~y) & ~(~x & y) expanding a) using b)
   */
  return ~(~(x & ~y) & ~(~x & y));
}
/* 
 * tmin - return minimum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmin(void) {
  /*
   * tmin is 1 0 0 ... 0
   * each right shift moves the 1 one place to the right; need 31 shifts
   * 0x1F is 31 (1 1 1 1 1)
   */
  return 1 << 0x1F;
}
//2
/*
 * isTmax - returns 1 if x is the maximum, two's complement number,
 *     and 0 otherwise 
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmax(int x) {
  /*
   * tmax      = 0 1 1 1 ... 1
   * tmax  + 1 = 1 0 0 0 ... 0
   * 2tmax + 1 = 1 1 1 1 ... 1
   *
   * Thus check that (x + 1) + x is all 1's, or that ~((x + 1) + 1) is all zeros:
   *
   * !(~((x + 1) + 1))
   *
   * However this is also true for -1:
   *
   * (2*-1 + 1) = -1 = 1 1 ... 1, so negating gives 0 0 ... 0
   *
   * To check for -1, we must have ~x is all zeroes, or:
   *
   * !(~x)
   *
   * Combine these two integer booleans with |
   */
  int allOnesIfTmax = (x + 1) + x;
  int isMinusOne = !(~x);

  return  !(~(allOnesIfTmax) | isMinusOne);

}
/* 
 * allOddBits - return 1 if all odd-numbered bits in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   Examples allOddBits(0xFFFFFFFD) = 0, allOddBits(0xAAAAAAAA) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int allOddBits(int x) {
  /*
   * Construct a mask of alternating 1010...10 for all 32-bits.
   * Then determine if all of those bits are set by applying the mask to x, and
   * asserting it is exactly the mask; ie that (x & mask) ^ mask = 0
   */
  // 0xAA = 1010 1010
  int a4 = (0xAA << 8) + 0xAA;
  int a8 = (a4 << 16) + a4;

  return !((x & a8) ^ a8);
}
/* 
 * negate - return -x 
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x) {
  return ~x + 1;
}
//3
/* 
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters '0' to '9')
 *   Example: isAsciiDigit(0x35) = 1.
 *            isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAsciiDigit(int x) {
  /*
   * The solution I came up with myself.
   * Use these patterns:
   *
   *   0x30 = 0011 0000 = 48
   *   0x39 = 0011 1001 = 57
   *  
   * First, check that the 28 MSB's are 00..0011
   * Then check that either the fourth bit is not set (handles 0-7), or that
   * the first four bits represent 8 or 9.
   */

  // 11 ... 1111 0000
  int upperMask = (1 << 31) >> 27; 
  // x & upperMask must == 0011 0000 == 0x30
  // Comparison trick: binary numbers are equal if (x ^ y) == 0
  // Thus if this value is not zero, the high bits are incorrect.
  int highBitsIncorrect = ((x & upperMask) ^ 0x30);


  // Now check lower 4 bits are 0 <= x <= 9
  // 0000   0
  // 0001   1
  // 0111   7  if the fourth bit is 0, we're <= 7
  // 1000   8  otherwise check if the bits are 8 or 9
  // 1001   9
  int firstFourBits = (x & 0xF);
  int zeroToSeven = !(firstFourBits & 0x8);

  return (!(highBitsIncorrect)) & (zeroToSeven | !(firstFourBits ^ 0x8) | !(firstFourBits ^ 0x9));
} 
int isAsciiDigit2(int x) {
  /*
   * Awesome solution from https://github.com/lmichalek/CodeSamples/blob/master/bit-manipulation/bits.c#L34
   * This is actually only 2 operations fewer than mine, however it exemplifies
   * cool bit tricks to see if a number is in a range.
   */
  int sign = 1 << 31;
  // If more than 0x39 is added to this number, it flips bits out to the sign
  // bit, resulting in a negative number.
  int upperBound = ~(sign | 0x39);
  // If 0x30 or less is added to this number, the result is negative,
  // otherwise it carry flips all the bits out to the sign bit, and becomes
  // positive.
  int lowerBound = ~0x30;

  upperBound = sign & (upperBound + x) >> 31;
  lowerBound = sign & (lowerBound + 1 + x) >> 31;

  return !(upperBound | lowerBound);
}
/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z) {
  /*
   * Convert x to 0 or 1: !!x
   *
   * Convert 1 to 0xFFFFFFFF and 0 to 0x0: ~x + 1
   * - 1 becomes 11..10 then add 1 to restore all 1's
   * - 0 becomes 11..11 then add 1 to overflow to all 0's
   * - Another way to do this is << 31 >> 31 (same number of operations)
   *
   * Intersect with the return values to select the correct one and zero out
   * the other
   *
   * Union the two branches for the final return value
   */
  x = !!x;   // x is 0 or 1
  x = ~x + 1; // 1 becomes all 1's, 0 becomes all 0's

  return (x & y) | (~x & z);
}
/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y) {
  /*
   * For most cases, check if y - x is negative. If so, x is > y.
   * Handle special cases/overflow by checking if x and y have different signs.
   * Overflow during subtraction can only occur if they have different signs
   * (negative number minus a positive => smaller, or a positive minus a
   * negative => larger).
   */
  int diffsign = x ^ y;            // sign bit is 1 if different signs
  int xnegypos = x & diffsign;     // sign bit is 1 if x is negative and y is positive
  int yminusxneg = (y + (~x + 1)); // sign bit is 1 if y - x < 0, which implies x > y

  // x <= y in one of two cases:
  // - x is negative and y is positive
  // - x is not negative and y - x is not negative
  //   ~diffsign & ~yminusxneg == ~(diffsign | yminusxneg) (DeMorgan's Law)
  return ((xnegypos | ~(diffsign | yminusxneg)) >> 31) & 1;
}
//4
/* 
 * logicalNeg - implement the ! operator, using all of 
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int logicalNeg(int x) {
  /*
   * If x is negative, need to return its sign bit. If x is not negative, add
   * it to tmax. If that result is negative, x > 1, so again return the sign bit.
   */

  int tmax = ~(1 << 31);
  // 1. MSB is 1 for all negative numbers: x | ...
  // 2. tmax + x, x > 0 will always be negative, so when x is non-neg, add it to
  //    tmax and look at the MSB
  return ~(x | (~x & (tmax + x))) >> 31 & 1;
}
int logicalNeg2(int x) {
  /*
   * Another approach: zero out everything except the lowest set bit. Only zero
   * will result-in all zeros. Take the two's complement of this:
   * - If x was 0:
   *   - mask = 0
   *   - two's complement = ~0 + 1 = 0 (overflows)
   * - If x was not zero:
   *   - mask has exactly one 1 in it: it is either positive of Tmin (100...0)
   *   - two's complement ~x + 1 is therefore negative (~Tmin + 1 = Tmin)
   *   - sign bit is 1
   */
  int leastsigbit = x & (~x + 1);
  int twoscomplement = ~leastsigbit + 1;
  int signbit = ~(twoscomplement >> 31);

  return signbit & 1;
}
/* howManyBits - return the minimum number of bits required to represent x in
 *             two's complement
 *  Examples: howManyBits(12) = 5
 *            howManyBits(298) = 10
 *            howManyBits(-5) = 4
 *            howManyBits(0)  = 1
 *            howManyBits(-1) = 1
 *            howManyBits(0x80000000) = 32
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 90
 *  Rating: 4
 */
int howManyBits(int x) {
  /*
   * This is equivalent to asking what is the MSB that _differs_ from the sign
   * bit; we'll need that many bits + one for the sign bit. For a negative number,
   * we're finding the highest 0 (b/c MSB is 1). For a positive number, we're
   * finding the highest 1.
   *
   * These can be converted to the same task by flipping negative numbers to their
   * one's complement. Then progressively look at half of the unevaluated bits by
   * shifting right. If there is a 1 in the shfited portion, we need at least that
   * many bits.
   */
  int sign = x >> 31;
  int b1, b2, b4, b8, b16;

  x = (sign & ~x) | (~sign & x);

  // If there was a 1 in the highest 16 bits, set b16 to 16 because at least 16
  // additional digits are required.
  b16 = !!(x >> 16) << 4;
  // If there was a 1 in the highest 16 bits, move them down because we're now
  // going to check the top 8 of _those_ bits. If there wasn't a 1 in the
  // highest 16 bits, b16 is 0, we don't shift x at all, and b8 will look for a
  // 1 in the top half of the original first 16 bits. This is basically binary
  // search.
  x = x >> b16;          
  b8 = !!(x >> 8) << 3;
  x = x >> b8;
  b4 = !!(x >> 4) << 2;
  x = x >> b4;
  b2 = !!(x >> 2) << 1;
  x = x >> b2;
  b1 = !!(x >> 1);
  x = x >> b1;

  return b16 + b8 + b4 + b2 + b1 + x + 1;
}
//float
/* 
 * floatScale2 - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatScale2(unsigned uf) {
  return 2;
}
/* 
 * floatFloat2Int - Return bit-level equivalent of expression (int) f
 *   for floating point argument f.
 *   Argument is passed as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point value.
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
int floatFloat2Int(unsigned uf) {
  return 2;
}
/* 
 * floatPower2 - Return bit-level equivalent of the expression 2.0^x
 *   (2.0 raised to the power x) for any 32-bit integer x.
 *
 *   The unsigned value that is returned should have the identical bit
 *   representation as the single-precision floating-point number 2.0^x.
 *   If the result is too small to be represented as a denorm, return
 *   0. If too large, return +INF.
 * 
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. Also if, while 
 *   Max ops: 30 
 *   Rating: 4
 */
unsigned floatPower2(int x) {
    return 2;
}
