#include <SWI-Prolog.h>
#include <string.h>

typedef struct {
  size_t sz;
  uint64_t *flows;
  uint64_t *dsts;
} graph_t;

typedef struct {
  size_t max_time;
  size_t max_agent;
  int *data;
} dp_table_t;

static void init_dp_table(
  graph_t *graph,
  int64_t time,
  int64_t who,
  dp_table_t *dp
) {
  dp->max_time = time + 1;
  dp->max_agent = who;
  size_t dp_sz =
    ((size_t) 1 << graph->sz)
    * (size_t) dp->max_time
    * (size_t) dp->max_agent
    * (size_t) graph->sz;
  dp->data = malloc(dp_sz * sizeof(int));
  memset(dp->data, -1, dp_sz * sizeof(int));
}
static void free_dp_table(dp_table_t *dp) {
  free(dp->data);
}

static int best_pressure(
  dp_table_t *dp,
  graph_t *graph,
  unsigned who,
  unsigned from,
  int time,
  unsigned mask
) {
  size_t key = mask;
  key *= dp->max_time; key += time;
  key *= graph->sz; key += from;
  key *= dp->max_agent; key += who - 1;
  int *memo = &dp->data[key];
  if (*memo != -1) {
    return *memo;
  }
  int best = 0;
  for (unsigned i = 1; i < graph->sz; i++) {
    if ((mask & (1 << i)) == 0) continue;
    int dist = graph->dsts[graph->sz * from + i];
    int new_time = time - dist - 1;
    if (new_time <= 0) continue;
    unsigned new_mask = mask & ~(1 << i);
    int here = new_time * graph->flows[i];
    here += best_pressure(dp, graph, who, i, new_time, new_mask);
    if (here > best) best = here;
  }
  if (who == 2) {
    int here = best_pressure(dp, graph, who - 1, 0, 26, mask);
    if (here > best) best = here;
  }
  return *memo = best;
}

foreign_t pl_best_pressure(
  term_t t_graph,
  term_t t_who,
  term_t t_time,
  term_t t_best
) {
  int64_t who, time;
  if (!PL_get_int64_ex(t_who, &who) ||
      !PL_get_int64_ex(t_time, &time)) PL_fail;
  graph_t graph;
  atom_t a_flow = PL_new_atom("flow");
  atom_t a_dsts = PL_new_atom("dsts");
  if (!PL_get_name_arity(t_graph, NULL, &graph.sz)) PL_fail;
  uint64_t flows[graph.sz], dsts[graph.sz * graph.sz];
  graph.flows = flows;
  graph.dsts = dsts;

  for (size_t i = 0; i < graph.sz; i++) {
    term_t t_src = PL_new_term_refs(4),
      t_flow = t_src + 1,
      t_dsts = t_src + 2,
      t_dst = t_src + 3;
    if (!PL_get_arg(i + 1, t_graph, t_src) ||
        !PL_get_dict_key(a_flow, t_src, t_flow) ||
        !PL_get_dict_key(a_dsts, t_src, t_dsts) ||
        !PL_get_uint64_ex(t_flow, &flows[i])) PL_fail;
    for (size_t j = 0; j < graph.sz; j++) {
      if (!PL_get_arg(j + 1, t_dsts, t_dst) ||
          !PL_get_uint64_ex(t_dst, &dsts[graph.sz * i + j])) PL_fail;
    }
    PL_reset_term_refs(t_src);
  }

  dp_table_t dp;
  init_dp_table(&graph, time, who, &dp);
  int best = best_pressure(
    &dp,
    &graph,
    (unsigned) who,
    0,
    (int) time,
    (1 << graph.sz) - 1
  );
  free_dp_table(&dp);

  if (!PL_unify_int64(t_best, best)) PL_fail;
  PL_succeed;
}

int main(int argc, char **argv) {
  PL_register_foreign("best_pressure", 4, pl_best_pressure, 0);
  return !PL_initialise(argc, argv);
}
