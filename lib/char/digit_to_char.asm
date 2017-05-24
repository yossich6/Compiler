/* char/digit_to_char.asm
 * 0 -> '0', ..., 9 -> '9'
 *
 * Programmer: Mayer Goldberg, 2010
 */

 DIGIT_TO_CHAR:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(1));
 JUMP_NE(L_INCORRECT_NUMBER_OF_ARGS_ERROR);
   MOV(R1, FPARG(2));
 CMP(IND(R1),IMM(945311));
 JUMP_NE(L_INCORRECT_TYPE);
  MOV(IND(R1),181048);
  MOV(R0,R1);
  POP(FP);
  RETURN;