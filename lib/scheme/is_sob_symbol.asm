 IS_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_SYMBOL);
  JUMP_EQ(L_IS_SOB_SYMBOL_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_SYMBOL_EXIT);
 L_IS_SOB_SYMBOL_TRUE:
  MOV(R0, IMM(1));
 L_IS_SOB_SYMBOL_EXIT:
  POP(FP);
  RETURN;


