(import :std/foreign)

(begin-ffi (mpz_new mpz_init mpz_init_set_si
            mpz_mul_si
            mpz_add mpz_mul
            mpz_tdiv_q
            mpz_cmp
            mpz_get_si)

  (c-declare "#include <gmp.h>")

  (c-define-type mpz_t* "mpz_ptr")
  (define-c-lambda mpz_new () mpz_t* "___return(malloc (sizeof(mpz_t)));")
  (define-c-lambda mpz_init (mpz_t*) void)
  (define-c-lambda mpz_init_set_si (mpz_t* long) void)
  (define-c-lambda mpz_mul_si (mpz_t* mpz_t* long) void)
  (define-c-lambda mpz_add (mpz_t* mpz_t* mpz_t*) void)
  (define-c-lambda mpz_mul (mpz_t* mpz_t* mpz_t*) void)
  (define-c-lambda mpz_tdiv_q (mpz_t* mpz_t* mpz_t*) void)
  (define-c-lambda mpz_cmp (mpz_t* mpz_t*) int)
  (define-c-lambda mpz_get_si (mpz_t*) long))
