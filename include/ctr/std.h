#ifndef STD_H
#define STD_H

struct ctr_call_ex;
struct ctr_state;

void ctr_usestd(struct ctr_state *state);

struct ctr_call_ex ctr_std_print(struct ctr_state *state);
struct ctr_call_ex ctr_std_println(struct ctr_state *state);
struct ctr_call_ex ctr_std_time(struct ctr_state *state);
struct ctr_call_ex ctr_std_error(struct ctr_state *state);
struct ctr_call_ex ctr_std_string(struct ctr_state *state);

struct ctr_call_ex ctr_std_new(struct ctr_state *state);
struct ctr_call_ex ctr_std_set(struct ctr_state *state);
struct ctr_call_ex ctr_std_get(struct ctr_state *state);

struct ctr_call_ex ctr_std_randi(struct ctr_state *state);
struct ctr_call_ex ctr_std_randf(struct ctr_state *state);

#endif // STD_H
