#include <caml/mlvalues.h>
#include <caml/memory.h>

value local_ref;

value caml_mpi_init_local_ref(value v)
{
  local_ref = v;
  caml_register_generational_global_root(&local_ref);

  return Val_unit;
}

value caml_mpi_get_local_ref(value unit)
{
  return local_ref;
}
