#include <SWI-Prolog.h>
#include <stdbool.h>
#include <stdio.h>
#include <limits.h>

#define MAT_C 4
#define INGR_C 3
#define GEODE 3

typedef struct { unsigned res[MAT_C]; } resources_t;
typedef resources_t pat_t;
typedef struct { unsigned counts[MAT_C]; } robots_t;
typedef struct { pat_t pats[MAT_C]; } blueprint_t;

static void recur(
  blueprint_t *bp,
  robots_t *max_robots,

  int time,
  resources_t resources,
  robots_t robots,

  unsigned *max_geodes
) {
  bool could_build = false;
  for (unsigned robot = 0; robot < MAT_C; ++robot) { // which robot to build
    if (robots.counts[robot] == max_robots->counts[robot]) continue;

    unsigned time_to_wait = 0;
    pat_t pat = bp->pats[robot];
    for (unsigned ingr = 0; ingr < INGR_C; ++ingr) { // limiting ingredient
      if (resources.res[ingr] >= pat.res[ingr]) continue;
      unsigned production = robots.counts[ingr];
      if (production == 0) goto cantbuild;
      unsigned required = pat.res[ingr] - resources.res[ingr];
      unsigned time_to_produce = (required / production) + (required % production != 0);
      if (time_to_produce > time_to_wait) time_to_wait = time_to_produce;
    }

    int new_time = time - (int) time_to_wait - 1;
    if (new_time <= 0) continue;

    robots_t new_robots = robots;
    new_robots.counts[robot]++;
    resources_t new_resources;
    for (unsigned i = 0; i < MAT_C; ++i) {
      new_resources.res[i]
        = resources.res[i]
        + robots.counts[i] * (time_to_wait + 1)
        - pat.res[i];
    }

    // if we built a geode robot every turn from now, could we beat the current max?
    if ((((new_time - 1) * new_time) / 2)
        + new_resources.res[GEODE]
        + new_time * new_robots.counts[GEODE]
        < *max_geodes) {
      continue;
    }

    could_build = true;
    recur(
      bp,
      max_robots,

      new_time,
      new_resources,
      new_robots,

      max_geodes
    );

  cantbuild:;
    continue;
  }

  if (!could_build) {
    unsigned geodes
      = resources.res[GEODE]
      + robots.counts[GEODE] * (unsigned) time;
    if (geodes > *max_geodes) *max_geodes = geodes;
  }
}

unsigned simulate_bp(
  blueprint_t *bp,
  int time
) {
  // there's no point building more of any robot than the most a recipe calls for
  robots_t max_robots;
  for (unsigned i = 0; i < INGR_C; ++i) {
    unsigned max = 0;
    for (unsigned j = 0; j < MAT_C; ++j) {
      unsigned required = bp->pats[j].res[i];
      if (required > max) max = required;
    }
    max_robots.counts[i] = max;
  }
  max_robots.counts[GEODE] = UINT_MAX;

  unsigned max_geodes = 0;
  recur(
    bp,
    &max_robots,
    time,
    (resources_t) {{ 0, 0, 0, 0 }},
    (robots_t) {{ 1, 0, 0, 0 }},
    &max_geodes
  );
  return max_geodes;
}

foreign_t pl_simulate_bp(
  term_t t_time,
  term_t t_blueprint,
  term_t t_res
) {
  uint64_t time;
  term_t t_pats = PL_new_term_ref();
  if (!PL_get_uint64_ex(t_time, &time) ||
      !PL_get_arg(1, t_blueprint, t_pats)) PL_fail;

  atom_t keys[] = {
    PL_new_atom("ore"),
    PL_new_atom("clay"),
    PL_new_atom("obsidian"),
    PL_new_atom("geode"),
  };

  blueprint_t blueprint;
  for (unsigned i = 0; i < MAT_C; i++) {
    term_t t_pat = PL_new_term_refs(5);
    if (!PL_get_arg(i + 1, t_pats, t_pat)) PL_fail;
    for (unsigned j = 0; j < MAT_C; j++) {
      term_t t_mat = t_pat + j + 1;
      uint64_t mat;
      if (!PL_get_dict_key(keys[j], t_pat, t_mat) ||
          !PL_get_uint64_ex(t_mat, &mat)) PL_fail;
      blueprint.pats[i].res[j] = (unsigned) mat;
    }
    PL_reset_term_refs(t_pat);
  }

  uint64_t res = simulate_bp(&blueprint, (int) time);
  if (!PL_unify_uint64(t_res, res)) PL_fail;

  PL_succeed;
}

int main(int argc, char **argv) {
  PL_register_foreign("simulate_bp", 3, pl_simulate_bp, 0);
  return !PL_initialise(argc, argv);
}
