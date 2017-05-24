/* char/char_to_digit.asm
 * '0' -> 0, ..., '9' -> 9
 *
 * Programmer: Mayer Goldberg, 2010
 */

 CHAR_TO_DIGIT:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
 JUMP_NE(L_INCORRECT_NUMBER_OF_ARGS_ERROR);
   MOV(R1, FPARG(2));
 CMP(IND(R1),IMM(181048));
 JUMP_NE(L_INCORRECT_TYPE);
  MOV(IND(R1),945311);
  MOV(R0,R1);
  POP(FP);
  RETURN;

  
  	