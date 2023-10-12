(import :std/foreign)

(begin-ffi (mpz_new
            mpz_init mpz_init_set_ui
            mpz_mul_ui mpz_submul_ui mpz_add mpz_addmul_ui
            mpz_tdiv_q
            mpz_cmp
            mpz_get_ui)

  (c-declare "#include <gmp.h>")

  (c-define-type mpz_t* "mpz_ptr")
  (define-c-lambda mpz_new () mpz_t* "___return(malloc (sizeof(mpz_t)));")
  (define-c-lambda mpz_init (mpz_t*) void)
  (define-c-lambda mpz_init_set_ui (mpz_t* unsigned-long) void)
  (define-c-lambda mpz_mul_ui (mpz_t* mpz_t* unsigned-long) void)
  (define-c-lambda mpz_submul_ui (mpz_t* mpz_t* unsigned-long) void)
  (define-c-lambda mpz_addmul_ui (mpz_t* mpz_t* unsigned-long) void)
  (define-c-lambda mpz_add (mpz_t* mpz_t* mpz_t*) void)
  (define-c-lambda mpz_tdiv_q (mpz_t* mpz_t* mpz_t*) void)
  (define-c-lambda mpz_cmp (mpz_t* mpz_t*) int)
  (define-c-lambda mpz_get_ui (mpz_t*) unsigned-long))
