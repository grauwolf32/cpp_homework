#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define SIZE 65
#define STRING_SIZE 256
#define LIMIT 9223372036854775807L

typedef unsigned long long ullong;

int get_primes(ullong number, ullong* array);
ullong read_number();

ullong read_number()
{
	char str[STRING_SIZE];
	size_t ptr = 0;
	size_t length = 0;
	ullong res = 0;

	scanf("%s",str);
	length = strlen(str);

	while(str[ptr] == ' ' && ptr < length)ptr++;
	if(ptr >= length){printf("[error]"); return -1;}

	while(ptr < length)
	{
		if(!(str[ptr] >= '0' && str[ptr] <= '9'))
		{
			printf("[error]");
			return -1;
		}
		ptr++;	 
	}
	sscanf(str,"%Lu",&res);
	if(res > LIMIT || res == 0){printf("[error]"); return -1;}
	return res;
}

int get_primes(ullong number, ullong* array)
{
	size_t ptr = 0;
	ullong num = number;
	ullong limit = (ullong)(sqrt((float)num + 1.0)) + 1;
	ullong div = 2;
	if(array == NULL){printf("[error]");return -1;}
	array[ptr++] = 1;
	
	while(num > 1 && div <= limit && ptr < SIZE)
	{
		while(num > 1 && num % div == 0 && ptr < SIZE){
			array[ptr++] = div;
			num /= div;
		}
		div++;
	}

	if(num != 1){array[ptr++] = num;}

	array[ptr] = 0;
	return 0;
}

int check_input()
{
	size_t i = 0;
	while(getc(stdin) == ' ')i++;

	if(getc(stdin) != EOF){
		printf("[error]");
		return -1;
	}

	return 0;
}

int main(void)
{
	ullong number = -1;
	ullong array[SIZE];

	size_t i = 0;
	
	number = read_number();
	if(number == -1){return 0;}
	
	if(check_input() == -1){return 0;}

	get_primes(number,array);
	while(array[i] != 0 && i < SIZE)printf("%Ld ",array[i++]);

	return 0;

}


