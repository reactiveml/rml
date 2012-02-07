#include <caml/mlvalues.h>
#include <caml/memory.h>

#define MAX_REF_NUMBER 3
value local_ref[MAX_REF_NUMBER];

value caml_mpi_init_local_ref(value idx, value v)
{
  CAMLparam2(idx, v);

  int i = Int_val(idx);
  local_ref[i] = v;
  caml_register_generational_global_root(&local_ref[i]);

  CAMLreturn(Val_unit);
}

value caml_mpi_get_local_ref(value idx)
{
  CAMLparam1(idx);
  int i = Int_val(idx);

  CAMLreturn(local_ref[i]);
}
