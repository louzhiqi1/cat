#ifndef NET_ENCRYPT_H
#define NET_ENCRYPT_H

#ifdef _WIN32
#	define WIN32_LEAN_AND_MEAN
#	include "Windows.h"
#endif

#include "erl_nif.h"
#include "encrypt.h"

#ifdef __cplusplus
extern "C" {
#endif

static ERL_NIF_TERM do_encrypt(int flag, ErlNifEnv *env, int argc, ERL_NIF_TERM argv[]);
static ERL_NIF_TERM net_encode(ErlNifEnv *env, int argc, ERL_NIF_TERM argv[]);
static ERL_NIF_TERM net_decode(ErlNifEnv *env, int argc, ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

#endif