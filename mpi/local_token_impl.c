#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/memory.h>
#include <mpi.h>

#define TOKEN_SIZE 1
#define TOKEN_TRUE 1
#define TOKEN_FALSE 0

#define Token_val(v) ((char*) Data_custom_val(v))

void token_serialize(value v, uintnat * wsize_32 /*size in bytes*/,
                     uintnat * wsize_64 /*size in bytes*/)
{
  caml_serialize_int_1(TOKEN_FALSE);
  *wsize_32 = TOKEN_SIZE;
  *wsize_64 = TOKEN_SIZE;
}

unsigned long token_deserialize(void* dst)
{
  caml_deserialize_uint_1();
  *((char*) dst) = TOKEN_FALSE;
  return TOKEN_SIZE;
}


static struct custom_operations token_ops = {
 identifier: "rml.locality_token",
 finalize:  custom_finalize_default,
 compare:     custom_compare_default,
 hash:        custom_hash_default,
 serialize:   token_serialize,
 deserialize: token_deserialize
};

void init_locality_token()
{
  caml_register_custom_operations(&token_ops);
}

value get_valid_locality_token(value a)
{
  CAMLparam1(a);
  CAMLlocal1(v);

  v = caml_alloc_custom(&token_ops, TOKEN_SIZE, 0, 1);
  /* set the token to true */
  *Token_val(v) = 1;

  CAMLreturn (v);
}

value is_valid_locality_token(value v)
{
  CAMLparam1(v);
  CAMLlocal1(res);

  res = Val_int(*Token_val(v));

  CAMLreturn (res);
}
