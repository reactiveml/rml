exception RML
exception End_program

type 'a clock = CkTop | CkLocal | CkExpr of 'a
type pause_kind = Strong | Weak
type signal_kind = Signal | Memory
