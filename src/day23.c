#include <SWI-Prolog.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

// I just adjusted these until they worked :)
#define TABLE_BREADTH 1024
#define TABLE_DEPTH 16
#define MAX_PROPOS 2048
#define DIR_COUNT 4

typedef struct { int32_t x, y; } pos_t;

typedef struct {
  pos_t entries[TABLE_DEPTH];
  size_t len;
} table_row_t;
typedef struct {
  table_row_t rows[TABLE_BREADTH];
} grid_t;
typedef struct {
  grid_t grid;
  size_t elfc;
  pos_t *elves;
} state_t;
typedef struct {
  pos_t to;
  size_t elf;
} proposition_t;

static size_t hash_pos(pos_t pos) { return (size_t) pos.x * 1123 + (size_t) pos.y; }
#define TABLE_ROW(GRID, POS) &(GRID)->rows[hash_pos((POS)) % TABLE_BREADTH];
static bool lookup_pos(grid_t *grid, pos_t pos) {
  table_row_t *row = TABLE_ROW(grid, pos);
  for (size_t i = 0; i < row->len; ++i) {
    pos_t found = row->entries[i];
    if (found.x == pos.x && found.y == pos.y) return true;
  }
  return false;
}
static void add_pos(grid_t *grid, pos_t pos) {
  table_row_t *row = TABLE_ROW(grid, pos);
  if (row->len == TABLE_DEPTH) {
    // we'd overwrite the next bucket otherwise...
    fprintf(stderr, "Max hash collisions exceeded\n");
    exit(1);
  }
  row->entries[row->len++] = pos;
}
static void remove_pos(grid_t *grid, pos_t pos) {
  table_row_t *row = TABLE_ROW(grid, pos);
  for (size_t i = 0; i < row->len; ++i) {
    pos_t found = row->entries[i];
    if (found.x == pos.x && found.y == pos.y) {
      if (i != row->len - 1) {
        row->entries[i] = row->entries[row->len - 1];
      }
      row->len--;
      return;
    }
  }
}

static int cmp_propo(const void *lhs_v, const void *rhs_v) {
  const proposition_t *lhs = lhs_v, *rhs = rhs_v;
  if (lhs->to.x < rhs->to.x) return -1;
  if (lhs->to.x > rhs->to.x) return 1;
  if (lhs->to.y < rhs->to.y) return -1;
  if (lhs->to.y > rhs->to.y) return 1;
  return 0;
}

static const pos_t directions[4][3] = {
  {{ 0, -1}, {-1, -1}, { 1, -1}},
  {{ 0,  1}, {-1,  1}, { 1,  1}},
  {{-1,  0}, {-1, -1}, {-1,  1}},
  {{ 1,  0}, { 1, -1}, { 1,  1}},
};

/*
static void print_state(state_t *state) {
  int
    min_x = INT_MAX,
    max_x = INT_MIN,
    min_y = INT_MAX,
    max_y = INT_MIN;
  for (size_t elf = 0; elf < state->elfc; ++elf) {
    pos_t pos = state->elves[elf];
    if (pos.x < min_x) min_x = pos.x;
    if (pos.x > max_x) max_x = pos.x;
    if (pos.y < min_y) min_y = pos.y;
    if (pos.y > max_y) max_y = pos.y;
  }

  puts("Grid:");
  for (int y = min_y; y <= max_y; ++y) {
    for (int x = min_x; x <= max_x; ++x) {
      char sym = lookup_pos(&state->grid, (pos_t) {x, y}) ? '#' : '.';
      putchar(sym);
    }
    puts("");
  }
}
*/

static void step_state(state_t *state, size_t *counter, size_t until) {
  proposition_t propos[MAX_PROPOS];
  size_t propoc;
  for (; *counter < until; ++*counter) {
    // print_state(state);
    propoc = 0;
    for (size_t elf = 0; elf < state->elfc; ++elf) {
      pos_t pos = state->elves[elf];
      for (int dx = -1; dx <= 1; ++dx) {
        for (int dy = -1; dy <= 1; ++dy) {
          if (dx == 0 && dy == 0) continue;
          pos_t nb_pos = { pos.x + dx, pos.y + dy };
          if (lookup_pos(&state->grid, nb_pos)) {
            goto has_nb;
          }
        }
      }
      continue;

    has_nb:;
      unsigned move_dir;
      for (unsigned ddir = 0; ddir < DIR_COUNT; ++ddir) {
        unsigned dir = (*counter + ddir) % DIR_COUNT;
        for (unsigned i = 0; i < 3; ++i) {
          pos_t dpos = directions[dir][i];
          pos_t nb_pos = { pos.x + dpos.x, pos.y + dpos.y };
          if (lookup_pos(&state->grid, nb_pos)) {
            goto cant_move;
          }
        }
        move_dir = dir;
        goto has_move;
      cant_move:;
        continue;
      }
      continue;
    has_move:;
      pos_t dpos = directions[move_dir][0];
      pos_t nb_pos = { pos.x + dpos.x, pos.y + dpos.y };
      proposition_t propo = { nb_pos, elf };
      propos[propoc++] = propo;
    }

    qsort(propos, propoc, sizeof(*propos), cmp_propo);
    size_t dst = 0;
    for (size_t src = 0; src < propoc; ++src) {
      pos_t to = propos[src++].to;
      bool add = true;
      for (; src < propoc; ++src) {
        pos_t to_o = propos[src].to;
        if (to.x != to_o.x || to.y != to_o.y) break;
        add = false;
      }
      src--;
      if (add) {
        propos[dst++] = propos[src];
      }
    }

    if (dst == 0) {
      ++*counter;
      return;
    }

    for (size_t pr = 0; pr < dst; ++pr) {
      proposition_t propo = propos[pr];
      pos_t *elf = &state->elves[propo.elf];
      remove_pos(&state->grid, *elf);
      *elf = propo.to;
      add_pos(&state->grid, *elf);
    }
  }
}

static foreign_t pl_step_state(
  term_t t_posns0,
  term_t t_counter0,
  term_t t_limit,
  term_t t_posns,
  term_t t_counter
) {
  uint64_t limit, counterv;
  if (!PL_get_uint64_ex(t_limit, &limit) ||
      !PL_get_uint64_ex(t_counter0, &counterv)) PL_fail;
  size_t counter = (size_t) counterv;

  state_t state; memset(&state, 0, sizeof(state));

  functor_t f_posns;
  if (!PL_get_functor(t_posns0, &f_posns)) PL_fail;
  state.elfc = PL_functor_arity(f_posns);
  pos_t elves[state.elfc];
  state.elves = elves;

  term_t
    t_posn = PL_new_term_ref(),
    t_pos_coord = PL_new_term_ref(),
    t_pos_coord1 = PL_new_term_ref();
  for (size_t elf = 0; elf < state.elfc; ++elf) {
    int64_t x_i, y_i;
    if (!PL_get_arg(elf + 1, t_posns0, t_posn) ||
        !PL_get_arg(1, t_posn, t_pos_coord) ||
        !PL_get_int64_ex(t_pos_coord, &x_i) ||
        !PL_get_arg(2, t_posn, t_pos_coord) ||
        !PL_get_int64_ex(t_pos_coord, &y_i)) PL_fail;
    pos_t pos = { (int) x_i, (int) y_i };
    elves[elf] = pos;
    add_pos(&state.grid, pos);
  }

  step_state(&state, &counter, (size_t) limit);

  if (!PL_unify_functor(t_posns, f_posns)) PL_fail;
  functor_t f_pair = PL_new_functor(PL_new_atom(","), 2);
  for (size_t elf = 0; elf < state.elfc; ++elf) {
    pos_t pos = state.elves[elf];
    if (!PL_put_int64(t_pos_coord, pos.x) ||
        !PL_put_int64(t_pos_coord1, pos.y) ||
        !PL_cons_functor(t_posn, f_pair, t_pos_coord, t_pos_coord1) ||
        !PL_unify_arg(elf + 1, t_posns, t_posn)) PL_fail;
  }

  if (!PL_unify_uint64(t_counter, (uint64_t) counter)) PL_fail;
  PL_succeed;
}

int main(int argc, char **argv) {
  PL_register_foreign("step_state", 5, pl_step_state, 0);
  return !PL_initialise(argc, argv);
}
