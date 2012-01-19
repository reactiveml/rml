#include <mpi.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/intext.h>

#define ACTIVE_WAITING

/* Various functions to initialize global variables */
value caml_mpi_get_comm_world_size(value unit)
{
  int size;
  CAMLparam1(unit);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  CAMLreturn (Val_int(size));
}

value caml_mpi_comm_rank(value unit)
{
  int rank;
  CAMLparam1(unit);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  CAMLreturn (Val_int(rank));
}

value caml_mpi_get_any_tag(value unit)
{
  return Val_int(MPI_ANY_TAG);
}

value caml_mpi_get_any_source(value unit)
{
  return Val_int(MPI_ANY_SOURCE);
}

/* Sending and receiving messages */
value caml_mpi_send(value data, value flags, value dest, value tag)
{
  char * buffer;
  long len;

  CAMLparam4(data, flags, dest, tag);

  output_value_to_malloc(data, flags, &buffer, &len);
  caml_release_runtime_system();
  MPI_Send(buffer, len, MPI_BYTE, Int_val(dest), Int_val(tag), MPI_COMM_WORLD);
  caml_acquire_runtime_system();
  caml_stat_free(buffer);

  CAMLreturn (Val_unit);
}

value caml_mpi_send_int(value data, value dest, value tag)
{
  long n;

  CAMLparam3(data, dest, tag);

  n = Long_val(data);
  caml_release_runtime_system();
  MPI_Send(&n, 1, MPI_LONG, Int_val(dest), Int_val(tag), MPI_COMM_WORLD);
  caml_acquire_runtime_system();

  CAMLreturn (Val_unit);
}

value caml_mpi_receive(value src, value tag)
{
  MPI_Status status;
  int count;
  value res;
  char * buffer;
#ifdef ACTIVE_WAITING
  int msg_received = 0;
#endif

  CAMLparam2(src, tag);

  caml_release_runtime_system();

  /* first probe to know the size of the value sent */
#ifdef ACTIVE_WAITING
  while (!msg_received) {
    MPI_Iprobe(Int_val(src), Int_val(tag), MPI_COMM_WORLD, &msg_received, &status);
    if (!msg_received) { usleep(50); };
  }
#else
  MPI_Probe(Int_val(src), Int_val(tag), MPI_COMM_WORLD, &status);
#endif
  /* allocate a buffer big enough */
  MPI_Get_count(&status, MPI_BYTE, &count);
  buffer = malloc(count);
  /* receive the value in this buffer */
  MPI_Recv(buffer, count, MPI_BYTE, Int_val(src), Int_val(tag), MPI_COMM_WORLD, &status);

  caml_acquire_runtime_system() ;

  res = caml_input_value_from_malloc(buffer, 0);

  CAMLreturn(res);
}

value caml_mpi_receive_int(value source, value tag)
{
  MPI_Status status;
  long n;

  CAMLparam2(source, tag);

  caml_release_runtime_system();
  MPI_Recv(&n, 1, MPI_LONG, Int_val(source), Int_val(tag), MPI_COMM_WORLD, &status);
  caml_acquire_runtime_system() ;

  CAMLreturn(Val_long(n));
}


