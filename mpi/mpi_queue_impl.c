#include <mpi.h>
#include <unistd.h>
#include <stdio.h>
#include <pthread.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>

#define POLLING_DELAY 25 /* in ms */
#define ALIGN_CACHE_LINE __attribute__((aligned(64)))

static struct custom_operations queue_ops = {
  "rml.queue",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

struct node { char* data; struct node* next; } ALIGN_CACHE_LINE;
struct queue {
  struct node volatile* hd; int msg_tag;
  pthread_t thread_id; int stop ALIGN_CACHE_LINE;
};

#define Queue_val(v) (*((struct queue **) Data_custom_val(v)))

/* add to the tail of the list*/
void add_fifo(char* data, struct node volatile**tl)
{
  struct node* new_n = (struct node*)malloc(sizeof(struct node));
  new_n->data = data;
  new_n->next = NULL;
  (*tl)->next = new_n;
  *tl = new_n;
}


char* mpi_receive(int tag)
{
  MPI_Status status;
  int count;
  char * buffer;
#ifdef SEMI_ACTIVE_WAITING
  int msg_received, nsec_start=1000, nsec_max=50000;
  struct timespec ts;
#endif

  /* first probe to know the size of the value sent */
#ifdef SEMI_ACTIVE_WAITING
  ts.tv_sec = 0;
  ts.tv_nsec = nsec_start;
  MPI_Iprobe(MPI_ANY_SOURCE, tag, MPI_COMM_WORLD, &msg_received, &status);
  while (!msg_received) {
    nanosleep(&ts, NULL);
    ts.tv_nsec *= 2;
    ts.tv_nsec = (ts.tv_nsec > nsec_max) ? nsec_max : ts.tv_nsec;
    MPI_Iprobe(MPI_ANY_SOURCE, tag, MPI_COMM_WORLD, &msg_received, &status);
  };
#else
  MPI_Probe(MPI_ANY_SOURCE, tag, MPI_COMM_WORLD, &status);
#endif
  /* allocate a buffer big enough */
  MPI_Get_count(&status, MPI_BYTE, &count);
  buffer = malloc(count);
  /* receive the value in this buffer */
  MPI_Recv(buffer, count, MPI_BYTE, status.MPI_SOURCE,
           status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

  return buffer;
}


/* Create a caml value representing the queue */
static value alloc_queue(struct queue *q)
{
  value v = alloc_custom(&queue_ops, sizeof(struct queue*), 0, 1);
  Queue_val(v) = q;
  return v;
}

/* Creates a new queue */
value mlmpi_mk_queue(value unit)
{
  CAMLparam1 (unit);
  /*allocate an empty node in the queue*/
  struct node *hd = (struct node*)malloc(sizeof(struct node));
  hd->next = NULL;
  hd->data = NULL;
  /*init the queue*/
  struct queue *q = (struct queue*)malloc(sizeof(struct queue));
  q->hd = hd;
  q->msg_tag = MPI_ANY_TAG;
  q->stop = 0;
  CAMLreturn (alloc_queue(q));
}

/* Function ran by the receiving thread */
void* receive_fun(void *arg)
{
  char* buffer;
  struct queue *q = (struct queue *) arg;
  struct node volatile* tl = q->hd;
  int tag = q->msg_tag;

  while(!q->stop) {
    buffer = mpi_receive(tag);
    add_fifo(buffer, &tl);
  }

  return NULL;
}

/* start receiving messages in the given queue */
value mlmpi_start_receiving(value msg_queue, value msg_tag)
{
  CAMLparam2(msg_queue, msg_tag);

  struct queue *q = Queue_val(msg_queue);
  q->msg_tag = Int_val(msg_tag);
  pthread_create(&q->thread_id, NULL, receive_fun, (void*) q);

  CAMLreturn (Val_unit);
}

value mlmpi_stop_receiving(value msg_queue)
{
  CAMLparam1(msg_queue);
  void* status;
  int rank;
  char c = '\0';

  struct queue *q = Queue_val(msg_queue);
  q->stop = 1;
  /* Send a dummy message to exit the send */
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Send(&c, 1, MPI_BYTE, rank, q->msg_tag, MPI_COMM_WORLD);
  /*Wait for the thread to terminate*/
  pthread_join(q->thread_id, &status);

  CAMLreturn (Val_unit);
}

void wait_non_empty(struct queue *q)
{
  int cpt=0;

  /* busy waiting */
  while(q->hd->next == NULL && cpt++ < 100) {}

  /* less-busy waiting */
   while(q->hd->next == NULL)
    usleep(POLLING_DELAY);

  return;
}


value mlmpi_queue_get(value msg_queue)
{
  CAMLparam1(msg_queue);
  CAMLlocal3(res, hd, tl);

  struct queue *q = Queue_val(msg_queue);
  struct node volatile* old_n;

  wait_non_empty(q);

  res = Val_emptylist;
  while(q->hd->next != NULL) {
    /* read next value in the list */
    old_n = q->hd;
    q->hd = q->hd->next;
    free((struct node*)old_n);
    /* store hd::res in res */
    hd = caml_input_value_from_malloc(q->hd->data, 0);
    q->hd->data = NULL;
    tl = res;
    res = caml_alloc(2, Tag_cons);
    Store_field(res, 0, hd);
    Store_field(res, 1, tl);
  }

  CAMLreturn(res);
}

