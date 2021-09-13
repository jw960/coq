#include <stdio.h>
#include <stdlib.h>

void ups_vm(const char *name) {
  fprintf(stderr, "Bytecode compiler called when it should not: %s\n", name);
  exit(1);
}

void ups_float(const char *name) {
  fprintf(stderr, "Primitive floats called when they should not: %s\n", name);
  exit(1);
}

/* Dummy wrappers,  */
void coq_push_vstack(void) { ups_vm(__func__); }
void coq_push_ra(void) { ups_vm(__func__); }
void coq_push_arguments(void) { ups_vm(__func__); }
void coq_push_val(void) { ups_vm(__func__); }
/* void coq_pushpop(void) { ups_vm(__func__); } */
void coq_pushpop(void) { }
void coq_set_drawinstr(void) { ups_vm(__func__); }
void coq_interprete_ml(void) { ups_vm(__func__); }
void coq_interprete_byte(void) { ups_vm(__func__); }
void coq_offset_closure(void) { ups_vm(__func__); }
void coq_closure_arity(void) { ups_vm(__func__); }
void coq_is_double(void) { ups_vm(__func__); }
void coq_offset(void) { ups_vm(__func__); }
void coq_eval_tcode(void) { ups_vm(__func__); }
void coq_tcode_of_code(void) { ups_vm(__func__); }
void coq_kind_of_closure(void) { ups_vm(__func__); }
void coq_int_tcode(void) { ups_vm(__func__); }
/* void coq_makeaccu(void) { ups_vm(__func__); } */
void coq_makeaccu(void) { }
void init_coq_vm(void) { ups_vm(__func__); }
void coq_is_accumulate_code(void) { ups_vm(__func__); }
void coq_set_bytecode_field(void) { ups_vm(__func__); }
void coq_tcode_array(void) { ups_vm(__func__); }
void coq_offset_tcode(void) { ups_vm(__func__); }
/* void accumulate_code(void) { ups_vm(__func__); } */
void accumulate_code(void) { }
void coq_next_up(void) { ups_vm(__func__); }
void coq_next_up_byte(void) { ups_vm(__func__); }
void coq_next_down(void) { ups_vm(__func__); }
void coq_next_down_byte(void) { ups_vm(__func__); }
void coq_shift_fix(void) { ups_vm(__func__); }
void coq_current_fix(void) { ups_vm(__func__); }
void coq_last_fix(void) { ups_vm(__func__); }

void coq_fsqrt(void) { ups_float(__func__); }
void coq_fsqrt_byte(void) { ups_float(__func__); }
void coq_fadd(void) { ups_float(__func__); }
void coq_fadd_byte(void) { ups_float(__func__); }
void coq_fsub(void) { ups_float(__func__); }
void coq_fsub_byte(void) { ups_float(__func__); }
void coq_fdiv(void) { ups_float(__func__); }
void coq_fdiv_byte(void) { ups_float(__func__); }
void coq_fmul(void) { ups_float(__func__); }
void coq_fmul_byte(void) { ups_float(__func__); }
void coq_uint63_to_float(void) { ups_float(__func__); }
void coq_uint63_to_float_byte(void) { ups_float(__func__); }
