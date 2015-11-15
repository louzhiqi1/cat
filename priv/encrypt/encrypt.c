#include "encrypt.h"
#include "memory.h"

#define DATA_MIN_LEN 4

void encode(BYTE * data, int len, BYTE * key)
{ 
	int i;

	data[0] = data[0] ^ key[0];

	for(i = 1;i < len; i++)
	{
		data[i] =  data[i] ^  data[i-1] ^ key[i&7];
	}
	
	data[3] = data[3] ^ key[2];
	data[2] = data[2] ^ data[3] ^ key[3];
	data[1] = data[1] ^ data[2] ^ key[4];
	data[0] = data[0] ^ data[1] ^ key[5];
}

void decode(BYTE * data, int len, BYTE * key)
{
	int count = len - 1;

	int i;

	data[0] = data[0] ^ data[1] ^ key[5];
	data[1] = data[1] ^ data[2] ^ key[4];
	data[2] = data[2] ^ data[3] ^ key[3];
	data[3] = data[3] ^ key[2];

	for(i = count; i > 0; i--)
	{
		data[i] =  data[i] ^ data[i-1] ^ key[i & 7];
	}

	data[0] = data[0] ^ key[0];
}

void change_key(BYTE * data, BYTE * key)
{
	DWORD *p_data = 0, *p_key = 0;

	p_data = (DWORD *)data;
	p_key  = (DWORD *)key;

	p_key[0] ^= *p_data;
	p_key[1] += 0xC3;
}

void send_encode(BYTE * data, int len, BYTE * key)
{
	if(len < DATA_MIN_LEN)
	{
		return;
	}

	encode(data, len, key);

	change_key(data, key);
}

void recv_decode(BYTE * data, int len, BYTE * key)
{
	BYTE old_key[KEY_LEN];

	if(len < DATA_MIN_LEN)
	{
		return;
	}

	memcpy(old_key, key, KEY_LEN);

	change_key(data, key);

	decode(data, len, old_key);
}