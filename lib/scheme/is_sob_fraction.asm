 IS_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_FRACTION);
  JUMP_EQ(L_IS_SOB_FRACTION_TRUE);
  MOV(R0, SOB_FALSE);
  JUMP(L_IS_SOB_FRACTION_EXIT);
 L_IS_SOB_FRACTION_TRUE:
  MOV(R0, SOB_TRUE);
 L_IS_SOB_FRACTION_EXIT:
  POP(FP);
  RETURN;


