#include "net_encrypt.h"

#define OPT_TYPE_ENCODE 0
#define OPT_TYPE_DECODE 1

#define BUFF_MAX_SIZE 100000

static ERL_NIF_TERM do_encrypt(int flag, ErlNifEnv *env, int argc, ERL_NIF_TERM argv[])
{
        unsigned int data_len;
        unsigned int chr_value;

        ERL_NIF_TERM *data_itor = NULL;

        unsigned int data_idx = 0;

        ERL_NIF_TERM data_head;
        ERL_NIF_TERM data_tail;

        ErlNifBinary data_bin;
        ErlNifBinary key_bin;

        ERL_NIF_TERM new_data;
        ERL_NIF_TERM new_key;

        if (argc != 2 || 
            !enif_inspect_binary(env, argv[0], &data_bin))
        {
                return enif_make_badarg(env);
        }

        if (!enif_inspect_binary(env, argv[1], &key_bin))
        {
                enif_release_binary(&data_bin);

                return enif_make_badarg(env);
        }

        switch (flag)
        {
        case OPT_TYPE_ENCODE:
                send_encode(data_bin.data, data_bin.size, key_bin.data);
                break;
        case OPT_TYPE_DECODE:
                recv_decode(data_bin.data, data_bin.size, key_bin.data);
                break;
        default:
                break;
        }

        new_data = enif_make_binary(env, &data_bin);
        new_key  = enif_make_binary(env, &key_bin);

        enif_release_binary(&data_bin);
        enif_release_binary(&key_bin);

        return enif_make_tuple(env, 2, new_data, new_key);
}

static ERL_NIF_TERM net_encode(ErlNifEnv *env, int argc, ERL_NIF_TERM argv[])
{
        return do_encrypt(OPT_TYPE_ENCODE, env, argc, argv);
}

static ERL_NIF_TERM net_decode(ErlNifEnv *env, int argc, ERL_NIF_TERM argv[])
{
        return do_encrypt(OPT_TYPE_DECODE, env, argc, argv);
}

static ErlNifFunc nif_funcs[] = {
        {"encode", 2, net_encode},
        {"decode", 2, net_decode}
};

ERL_NIF_INIT(net_encrypt, nif_funcs, NULL, NULL, NULL, NULL)