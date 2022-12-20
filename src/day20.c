#include <SWI-Prolog.h>

typedef struct node_t node_t;
struct node_t {
  int64_t data;
  node_t *prev, *next;
};

void mixtimes(size_t times, size_t sz, node_t *nodes) {
  int64_t base = (int64_t) sz - 1;
  for (size_t t = 0; t < times; ++t) {
    for (size_t i = 0; i < sz; ++i) {
      node_t *node = &nodes[i];
      node->prev->next = node->next;
      node->next->prev = node->prev;
      int64_t mvby0 = node->data % base;
      if (mvby0 < 0) mvby0 += base;
      size_t mvby = (size_t) mvby0;
      node_t *prev = node->prev;
      for (size_t j = 0; j < mvby; ++j) {
        prev = prev->next;
      }
      node_t *next = prev->next;
      node->prev = prev;
      node->next = next;
      prev->next = node;
      next->prev = node;
    }
  }
}

foreign_t pl_mixtimes(
  term_t t_times,
  term_t t_nums0,
  term_t t_nums
) {
  uint64_t times;
  if (!PL_get_uint64_ex(t_times, &times)) PL_fail;
  size_t sz;
  if (!PL_get_name_arity(t_nums0, NULL, &sz)) PL_fail;

  node_t nodes[sz];
  size_t zero = 0;
  term_t t_data = PL_new_term_ref();
  for (size_t i = 0; i < sz; ++i) {
    node_t *node = &nodes[i];
    if (!PL_get_arg(i + 1, t_nums0, t_data) ||
        !PL_get_int64_ex(t_data, &node->data)) PL_fail;
    if (node->data == 0) zero = i;
    node->prev = &nodes[(sz + i - 1) % sz];
    node->next = &nodes[(i + 1) % sz];
  }
  mixtimes(times, sz, nodes);

  functor_t f_ret = PL_new_functor(PL_new_atom("f"), (int) sz);
  if (!PL_unify_functor(t_nums, f_ret)) PL_fail;
  node_t *node = &nodes[zero];
  for (size_t i = 0; i < sz; ++i) {
    if (!PL_put_int64(t_data, node->data) ||
        !PL_unify_arg(i + 1, t_nums, t_data)) PL_fail;
    node = node->next;
  }
  PL_succeed;
}

int main(int argc, char **argv) {
  PL_register_foreign("mixtimes", 3, pl_mixtimes, 0);
  return !PL_initialise(argc, argv);
}
