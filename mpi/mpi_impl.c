#include <mpi.h>
#include <unistd.h>
#include <time.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/intext.h>

//#define SEMI_ACTIVE_WAITING

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

  /** Passive send (does not improve performance)
  int flag, nsec_start=1000, nsec_max=100000;
  struct timespec ts;
  MPI_Request req;
  MPI_Status status;


  ts.tv_sec = 0;
  ts.tv_nsec = nsec_start;

  PMPI_Isend(buffer, len, MPI_BYTE, Int_val(dest), Int_val(tag), MPI_COMM_WORLD, &req);
  do {
    nanosleep(&ts, NULL);
    ts.tv_nsec *= 2;
    ts.tv_nsec = (ts.tv_nsec > nsec_max) ? nsec_max : ts.tv_nsec;
    PMPI_Request_get_status(req, &flag, &status);
  } while (!flag);
  **/

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
  int count, msrc, mtag;
  value res;
  char * buffer;
#ifdef SEMI_ACTIVE_WAITING
  int msg_received, nsec_start=1000, nsec_max=100000;
  struct timespec ts;
#endif

  CAMLparam2(src, tag);

  msrc = Int_val(src);
  mtag = Int_val(tag);

  caml_release_runtime_system();

  /* first probe to know the size of the value sent */
#ifdef SEMI_ACTIVE_WAITING
  ts.tv_sec = 0;
  ts.tv_nsec = nsec_start;
  MPI_Iprobe(msrc, mtag, MPI_COMM_WORLD, &msg_received, &status);
  while (!msg_received) {
    nanosleep(&ts, NULL);
    ts.tv_nsec *= 2;
    ts.tv_nsec = (ts.tv_nsec > nsec_max) ? nsec_max : ts.tv_nsec;
    MPI_Iprobe(msrc, mtag, MPI_COMM_WORLD, &msg_received, &status);
  };
#else
  MPI_Probe(msrc, mtag, MPI_COMM_WORLD, &status);
#endif
  /* allocate a buffer big enough */
  MPI_Get_count(&status, MPI_BYTE, &count);
  buffer = malloc(count);
  /* receive the value in this buffer */
  MPI_Recv(buffer, count, MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

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

value caml_mpi_receive_int_timeout(value source, value tag, value timeout)
{
  MPI_Status status;
  long n;
  int flag;

  CAMLparam3(source, tag, timeout);

  caml_release_runtime_system();
  MPI_Iprobe(Int_val(source), Int_val(tag), MPI_COMM_WORLD, &flag, &status);
  if(flag) /* message received */
    MPI_Recv(&n, 1, MPI_LONG, Int_val(source), Int_val(tag), MPI_COMM_WORLD, &status);
  else {
    usleep(Int_val(timeout));
    MPI_Iprobe(Int_val(source), Int_val(tag), MPI_COMM_WORLD, &flag, &status);
    if(flag) /* message received */
      MPI_Recv(&n, 1, MPI_LONG, Int_val(source), Int_val(tag), MPI_COMM_WORLD, &status);
    else
      n = -1; /* signal the absence of message */
  }
  caml_acquire_runtime_system() ;

  CAMLreturn(Val_long(n));
}
