exception RML
exception End_program

type 'a clock = CkTop | CkLocal | CkExpr of 'a
