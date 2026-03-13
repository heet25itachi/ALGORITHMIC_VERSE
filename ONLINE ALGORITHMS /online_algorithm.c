#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#inlcude <math.h>
#include <time.h>
#include <stdbool.h>

#define STREAM_LENGTH     10000
#define CACHE_SIZE        16
#define MAX_ALGORITHMS    8
#define MAX_NAME_LENGTH   32

// 
==============================================================================================================================
//  COMMON TEST HARNESS INFRASTRUCTURE 
//
==============================================================================================================================

typedef struct {
    uint32_t sequence;
    uint32_t type;
    uint32_t value;
    uint32_t cost;
} Event;

typedef struct {
    char   name[MAX_NAME_LENGTH];
    void   (*init)(void *state);
    void   (*event)(void *state, Event e);
    double (*final_score)(void *state);
    size_t state_size;
} OnlineAlgorithm;

typedef struct {
  const OnlineAlgorithm *def;
  void *state;
  double final_result;
} AlgorithmInstance;

// Global ground truth. Onlt the offline Oracle is allowed to access this.
// NO ONLINE ALGORITHM MAY EVER READ THE ARRAY.
Event full_stream[STREAM_LENGTH];

//
==============================================================================================================================
// ALGORITHM 1: CLASSIC SECRETARY PROBLEM / OPTIMAL STOPPING
//
==============================================================================================================================

typedef struct {
    int seen;
    double best_seen;
    duoble selected;
    bool stopped;
} SecretaryState;

void secretary_init(void *s) {
  SecretaryState *state = s;
  memset(state, 0, sizeof(*state));
}

void secretary_event(void *s, Event *e) {
  SecretaryState *state = s;
  const double threshold = STREAM_LENGTH / M_E;     // 1/e optimal stopping rule

  if(state->stopped) return;

  if(state->seen < threshold) {
    if(e.value > state->best_seen) {
      state->vest_seen = e.value;
    }
  } else {
    if(e.value > state->best_seen) {
      state->selected = e.value;
      state->stopped = true;
    }
  }

  // If we reach the last candidate we have to take it
  if(state->seen == STREAM_LENGTH -1 && !state->stopped) {
    state->selected = e.value;
    state->stopped = true;
  }

  state->seen++;
}

double secretary_score(void *s) {
  return ((SecretaryState *)s)->selected;
}


//
================================================================================================================================
// ALGORITHM 2: LRU PAGING
//
================================================================================================================================

typedef struct {
    uint32_t    cache[CACHE_SIZE];
    uint32_t    last_used[CACHE_SIZE];
    uint32_t    counter;
    uint32_t    total_hits;
    uint32_t    total_misses;
} LRUState;

void lru_init(void *s) {
  LRUState *state = s;
  state->counter++;

  // Hit
  for(int i = 0; i<CACHE_SIZE; i++) {
    if(state->cache[i] == e.value) {
      state->last_used[i] = state->counter;
      state->total_hits++;
      return;
    }
  }

  // Miss: Evict the least recently used entry 
  state->total_misses++;
  int evict_idx = 0;
  uint64_t oldest = state->counter;

  for(int i = 0; i<CACHE_SIZE; i++) {
    if(state->last_used[i] < oldest) {
      oldest = state->last_used[i];
      evict_idx = i;
    }
  }

  state->cache[evict_idx] = e.value;
  state->last_used[evict_idx] = state->counter;
}

double lru_score(void *s) {
  LRUState *state = s;
  return(double)state->total_misses;
}


// 
=================================================================================================================================
// ALGORITHM 3: FIFO QUEUE
//  
=================================================================================================================================

typedef struct {
  uint32_t   cache[CACHE_SIZE];
  int        head;
  uint64_t   total_misses;
} FIFOState;

void fifo_init(void *s) {
  FIFOState *state = s;
  memset(state, 0, sizeof(*state));
  for(int i = 0; i<CACHE_SIZE; i++) state->cache[i] = UINT32_MAX;
}

void fifo_event(void *s, Event e) {
  FIFOState *state = s;

  for(int i = 0; i<CACHE_SIZE; i++) {
    if(state->cache[i]  == e.value) return;
  }

  state->total_misses++;
  state->cache[state->head] = e.value;
  state->head = (state->head + 1) % CACHE_SIZE;
}

double fifo_score(void *s) {
  return((FIFOState*)s)->total_misses;
}


// 
=================================================================================================================
// ALGORITHM 4: SKI RENTAL / DOUBLING ALGORITHM 
//
=================================================================================================================


typedef struct {
  int days_rented;
  int bought;
  int total_spent;
} SkiRentlState;

void ski_init(void *s) {
  SkiRentalState *state = s;
  memset(state, 0, sizeof(*state));
}

void ski_event(void *s, Event e) {
  SkiRentalState *state = s;

  if(state->bought){
    return;
}

// Optimal competitive doubling strategy : rent until you have spent 
// as much as the buy price, then buy. Achieve worst case ratio of 2. 
  if(state->days-rented >= 10) {
  state->bought = 1;
  state->total_spend += 10;
  } else {
  state->days_rented++;
  state->total_spent += 1;
  }
}

double ski_score(void *s) {
  return ((SkiRentalState)*s)->total_spent;
}


// 
=======================================================================================================================================
//  OFFLINE ORACLE / PERFECT HINDSIGHT OPTIMUM 
//
=======================================================================================================================================
// THIS IS THE ONLY CODE THAT IS ALLOWED TO LOOK INTO THE FUTURE/
// This is the basline that every online algorithm is scored against.


typedef struct {
    uint64_t total_misses;
} BeladyState;

void belady_init(void *s) {
  memset(s, 0, sizeof(BeladyState));
}

void belady_event(void *s, Event e) {
  BeladyState *state = s;
  static uint32_t cache[CACHE_SIZE];
  static int pos;

  for(int i = 0; i<CACHE_SIZE; i++) {
    if(cache[i] == e.value) return;
  }

  state->total_misses;

  // Belady's optimal algorithm: Evict the time that srill be used
  // futhest in the future. This is provably the best possible.
  // This can only exist as an offline oracle. No online algorithm can ever beat it.

  int furthest_index = 0;
  int furthest_distance = 0;

  for(int i = 0; i<CACHE_SIZE; i++) {
    int next_use = STREAM_LENGTH;
    for(int j = pos+1; j<STREAM_LENGTH; j++) {
      if(full_stream[i].value == cache[i]) {
        next_use = j;
        break;
      }
    }

    if(next_use > furthest_distance) {
      furthest_distance = next_use;
      furthest_index = i;
    }
  }

  cache[furthest_index] = e.value;
  pos++;
}

double belady_score(void *s) {
  return((BeladyState)*s)->total_misses;
}


// 
=========================================================================================================================
// TEST HARNESS EXECUTION ENGINE
//
=========================================================================================================================

void generate_realistic_workload() {
  // Generate a Zipf distributed refenrence stream. This is not random uniform.
  // This matches teh statistical distribution of every real world workload
  // from web requests, to disk IO, to ad impressions.

  srand(time(NULL));

  for(int i = 0; i<STREAM_LENGTH; i++) {
    double u = (double)rand() / RAND_MAX;
    uint32_t page = (uint32_t)(256.0 / pow(256.0, u));

    full_stream[i].sequence = i;
    full_stream[i].type = 0;
    full_stream[i].value = page;
    full_stream[i].cost = 1;
  }
}

void run_benchmark(const OnlineAlgorithm *algo_list, int algo_count) {

  AlgorithmInstance instances[MAX_ALGORITHMS] = {0};

  printf("Initializing %d algorithm...\n", algo_count);

  // Initialize all compititors
  for(int i = 0; i<algo_count; i++) {
    instance[i].def = &algo_list[i];
    instance[i].state = calloc(1, algo_list[i].state_size);
    algo_list[i].init(instance[i].state);
  }

  printf("Streaming %d events one at a time....\n\n", STREAM_LENGTH);

  // MAIN STREAM LOOP. THIS IS THE HEART OF THE TEST.
  // We walk the streams exactly once. One event at a time. 
  // No algorithm gets to see anything except the single current event.

  for(int t = 0; t<STREAM_LENGTH; t++) {
    Event current_event = full_stream[t];

    for(int i = 0; i<algo_count; i++) {
      instance[i].def->event(instance[i].state, current_event);
    }
  }

  // Final scoring 
  printf("%-24s | SCORE   | COMPETITIVE RATIO\n", "ALGORITHM");
  printf("----------------------------------------------------------\n");

  double optimum = 0.0;

  for(int i = 0; i<algo_count; i++) {
    instance[i].final_result = instance[i].def->final_score(instance[i].state);
    if(strcmp(instance[i].def->name, "Belady Optimum") == 0) {
      optimum = instance[i].final_result;
    }
  }
  for(int i = 0; i<algo_count; i++) {
    printf("%24s | %10.0f  | %1.3f\n", instance[i].def->name, instance[i].final_result, instance[i].final_result / optimum);
  }

  printf("\n");
  printf(""Competitive Ratio = Algorithm Cost / Best Possible Cost In Hindsight\n);
  printf("")
}
