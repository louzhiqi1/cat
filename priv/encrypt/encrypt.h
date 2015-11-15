#ifndef __ENCRYPT_H__
#define __ENCRYPT_H__

typedef unsigned char BYTE;
typedef unsigned long DWORD;

#define KEY_LEN 8

#ifdef __cplusplus
extern "C" {
#endif

void encode(BYTE * data, int len, BYTE * key);
void decode(BYTE * data, int len, BYTE * key);
void change_key(BYTE * data, BYTE * key);
void send_encode(BYTE * data, int len, BYTE * key);
void recv_decode(BYTE * data, int len, BYTE * key);

#ifdef __cplusplus
}
#endif

#endif