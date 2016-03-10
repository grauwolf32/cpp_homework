#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAXLEN 1000
#define SYS 10000
#define LOGSYS 4

#define log(s, ...) printf(s " (%s in %s:%d)", __VA_ARGS__, __func__, \
    __FILE__, __LINE__)

typedef struct{
	int val[MAXLEN+1];
	int st;
}vlong;

typedef struct {
	char* 	buf;
	size_t size;
	size_t  pos;
}buffer;

void add(vlong *op1, vlong *op2, vlong *res);
void mul(vlong *op1, int sh,int offs, vlong *res);
void mul_long(vlong *op1, vlong *op2, vlong *res);
void div_long(vlong *op1,int sh, vlong *res);

int  geq(vlong* op1,vlong* op2);
int  leq(vlong* op1,vlong* op2);
int  les(vlong* op1,vlong* op2);
int  grt(vlong* op1,vlong* op2);
int  equ(vlong* op1,vlong* op2);

void print_vlong(vlong *c);
int reads_vlong(vlong *c, char* str);
int  read_vlong(vlong *c);

void resize_buff(buffer* r,size_t n);
int  add_to_buff(buffer* r, char* s);
void init_buff(buffer* r,size_t n);
void delete_buff(buffer* r);


int make_tests();
int equ_vlong_test();
int add_vlong_test();
int sub_vlong_test();
int mul_vlong_test();
int div_vlong_test();


int main(void)
{
	make_tests();
	return 0;
}

void print_vlong(vlong *c)
{
	int i;
	printf(" %d ",c->val[c->st]);
	for(i = c->st + 1; i <= MAXLEN;i++)
		printf("% .4d ",c->val[i]);
}

int read_vlong(vlong *c)
{
	int   st = MAXLEN, temp;
	char  str[MAXLEN + 1];
	char  r_val[LOGSYS+1];
	char* str_pos;
	char  tmp = '\0';

	scanf("%s",str);
	size_t length   = strlen(str);
	size_t sections = (size_t)(length/LOGSYS);
	size_t rest  = length % LOGSYS;

	int i = sections-1;

	while(i >= 0){
		str_pos = (char*)(str + i*LOGSYS +rest);
		if(sscanf(str_pos,"%4d",&temp) != 1){printf("[error]");return -1;} /*If LOGSYS changing, don't forget to change %4d */
		c->val[st--] = temp;
		i--;
	}

	if(rest)
	{
		strncpy(r_val,str,rest);
		r_val[rest] = '\0';

		sscanf(r_val,"%d",&temp);
		c->val[st--] = temp;
	}

	c->st = st + 1;

	if(c->val[c->st] < 0)
	{
		for(i = c->st + 1;i <= MAXLEN;i++)
			c->val[i] *= -1;
	}
	return 0;
}

int reads_vlong(vlong *c, char* str)
{
	int   st = MAXLEN, temp;
	char  r_val[LOGSYS+1];
	char* str_pos;
	char  tmp = '\0';

	size_t length   = strlen(str);
	size_t sections = (size_t)(length/LOGSYS);
	size_t rest  = length % LOGSYS;

	int i = sections-1;

	while(i >= 0){
		str_pos = (char*)(str + i*LOGSYS +rest);
		if(sscanf(str_pos,"%4d",&temp) != 1){printf("[error]");return -1;} /*If LOGSYS changing, don't forget to change %4d */
		c->val[st--] = temp;
		i--;
	}

	if(rest)
	{
		strncpy(r_val,str,rest);
		r_val[rest] = '\0';

		sscanf(r_val,"%d",&temp);
		c->val[st--] = temp;
	}

	c->st = st + 1;

	if(c->val[c->st] < 0)
	{
		for(i = c->st + 1;i <= MAXLEN;i++)
			c->val[i] *= -1;
	}
	return 0;
}

void add(vlong *op1, vlong *op2, vlong *res)
{
	vlong *mxop, *mnop;
	int i, flag = 0,st;

	mxop = op1->st >  op2->st ? op1 : op2;
	mnop = op1->st <= op2->st ? op1 : op2;
	st = mnop->st;

	for(i=MAXLEN; i >= mxop->st;i--){
		res->val[i] = mxop->val[i] + mnop->val[i] + flag;
		flag = res->val[i] / SYS;
		res->val[i] %= SYS;
	}

	for(i=mxop->st-1; i >= mnop->st;i--){
		res->val[i] = mnop->val[i] + flag;
		flag = res->val[i] / SYS;
		res->val[i] %= SYS;
	}

	if(flag) res->val[--st] = flag;
	res->st = st;
	
	while(res->val[res->st] == 0 && res->st < MAXLEN)
		res->st += 1;
}

void mul(vlong *op1, int sh,int offs, vlong *res)
{
	int i, flag = 0;
	int st = op1->st;

	for(i=MAXLEN-offs+1; i <= MAXLEN;i++)
		res->val[i] = 0;

	for(i=MAXLEN; i >= st; i--){
		res->val[i-offs] = op1->val[i] * sh + flag;
		flag = res->val[i-offs] / SYS;
		res->val[i-offs] %= SYS;
	}

	if(flag) res->val[--st-offs] = flag;
	res->st = st-offs;

	while(res->val[res->st] == 0 && res->st < MAXLEN)
		res->st += 1;
}

void mul_long(vlong *op1, vlong *op2, vlong *res)
{
	int i;
	vlong temp;

	res->st = MAXLEN;
	res->val[MAXLEN] = 0;
	for(i = MAXLEN; i >= op1->st;i--){
		mul(op2,op1->val[i],MAXLEN-i,&temp);
		add(res,&temp,res);
	}

	while(res->val[res->st] == 0 && res->st < MAXLEN)
		res->st += 1;
}

void sub(vlong *op1,vlong *op2,vlong *res)
{
	vlong temp;
	mul(op2, -1, 0, &temp);
	add(op1, &temp, res);

	while(res->val[res->st] == 0 && res->st < MAXLEN)
		res->st += 1;	
}

int equ(vlong* op1, vlong* op2)
{
	if(op1->st != op2->st)
		return 0;

	int st = op1->st;
	int i = 0;
	for(i = st;i <= MAXLEN;i++)
	{
		if(op1->val[i] == op2->val[i])
			continue;
		else return 0;	
	}
	return 1;
}

int  geq(vlong* op1,vlong* op2)
{
	if(op1->st < op2->st) return 1;
	if(op1->st > op1->st) return 0;
	
	int st = op1->st;
	int i = 0;

	for(i = st;i <= MAXLEN;i++)
	{
		if(op1->val[i] >= op2->val[i])
			continue;
		else return 0;
	}
	return 1;
}
int  leq(vlong* op1,vlong* op2)
{
	if(op1->st < op2->st) return 0;
	if(op1->st > op1->st) return 1;
	
	int st = op1->st;
	int i = 0;

	for(i = st;i <= MAXLEN;i++)
	{
		if(op1->val[i] <= op2->val[i])
			continue;
		else return 0;
	}
	return 1;
}
int  les(vlong* op1,vlong* op2)
{
	return 1 - geq(op1,op2);
}

int  grt(vlong* op1,vlong* op2)
{
	return 1 - leq(op1,op2);
}


/* TODO Make the shit below works well */

void div_long(vlong *op1,int sh, vlong *res)
{
	int i, flag = 0;
	int j = MAXLEN;
	int st = op1->st;
	long long temp = 0;

	for(i=st; i <= MAXLEN;i++)
		res->val[i] = 0;

	for(i=MAXLEN; i >= st;i--)
	{
		if(op1->val[i] + flag < sh)
		{
			temp = op1->val[i]*SYS + op1->val[i-1] + flag;
			res->val[j] = temp / sh;
			flag = (temp % sh)*SYS;
			i--;
			j--;
		}
		
		temp = op1->val[i] + flag;
		res->val[j] = (int)(temp / sh);
		flag = (temp % sh)*SYS;
		j--;
	}
	res->st = j + 1;
}


/* ------------------------------------------------------------------------------------------ */

/*-----------Here we build function to eleminate all spaces and '\n's in the text-------------*/
void resize_buff(buffer* r,size_t n)
{
	r->buf = (char*)realloc(r->buf,n*sizeof(char));
	r->size = n;
}
void init_buff(buffer* r,size_t n)
{
	r->buf = (char*)malloc(n*sizeof(char));
	r->size = n;
	r->pos = 0;
}
void delete_buff(buffer* r)
{
	if(r->buf != NULL)
		free(r->buf);
	r->buf = NULL;
	r->size = 0;
	r->pos  = 0;
}
int add_to_buff(buffer* r, char* s)
{
	size_t len = strlen(s); 
	size_t count = 0;
	size_t i = 0;
	if(len <= 0)return 0;

	for(i = 0;i < len;i++)
	{	
		if(s[i] != ' ' && s[i] != '\n')
		{
			r->buf[r->pos++] = s[i];
			count++;
		}
		else continue;
	}
	return count;	
}

/* ------------------------------------------------------------------------------------------ */
/* ------------------------------------------Tests------------------------------------------- */

int equ_vlong_test()
{
	vlong a,b,c,e;
	size_t test_num = 0;
	log("Test %zu",test_num++);
		reads_vlong(&a,"0");
		reads_vlong(&b,"0");
	if(!equ(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111111111");
		reads_vlong(&b,"111111111111111");
	if(!equ(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111111111");
		reads_vlong(&b,"111111111111111");
	if(!geq(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111111111");
		reads_vlong(&b,"111111111111111");
	if(!leq(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++); /*FAIL*/
		reads_vlong(&a,"0");
		reads_vlong(&b,"111111111111111");
	if(!leq(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&b,"-1");
		reads_vlong(&a,"111111111111111");
	if(!geq(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	log("Test %zu",test_num++);
		reads_vlong(&a,"0");
		reads_vlong(&b,"1111");
	if(!les(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	log("Test %zu",test_num++); /*FAIL*/
		reads_vlong(&a,"100000");
		reads_vlong(&b,"1111");
	if(!grt(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	return 0;
}
int add_vlong_test()
{
	vlong a,b,c,e;
	size_t test_num = 0;

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111111111");
		reads_vlong(&b,"111111111111111");
		reads_vlong(&e,"222222222222222");
		add(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"0");
		reads_vlong(&b,"111111111111111");
		reads_vlong(&e,"111111111111111");
		add(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111111111");
		reads_vlong(&b,"-111111111111111");
		reads_vlong(&e,"0");
		add(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	return 0;
}
int sub_vlong_test()
{
	vlong a,b,c,e;
	size_t test_num = 0;

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111111111");
		reads_vlong(&b,"111111111111111");
		reads_vlong(&e,"0");
		sub(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"1");
		reads_vlong(&b,"11");
		reads_vlong(&e,"-10");
		sub(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111111111");
		reads_vlong(&b,"-111111111111111");
		reads_vlong(&e,"222222222222222");
		sub(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	return 0;
}
int mul_vlong_test()
{
	vlong a,b,c,e;
	size_t test_num = 0;

	log("Test %zu",test_num++);
		reads_vlong(&a,"1");
		reads_vlong(&b,"0");
		reads_vlong(&e,"0");
		mul_long(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"-111111111");
		reads_vlong(&e, "111111111");
		mul(&a,-1,0,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"1");
		reads_vlong(&e,"0");
		mul(&a,0,0,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"37");
		reads_vlong(&b,"3");
		reads_vlong(&e,"111");
		mul_long(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	log("Test %zu",test_num++);
		reads_vlong(&a,"-111111111111111");
		reads_vlong(&b,"-1");
		reads_vlong(&e,"111111111111111");
		mul_long(&a,&b,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");	

	return 0;
}
int div_vlong_test()
{
	vlong a,b,c,e;
	size_t test_num = 0;

	log("Test %zu",test_num++);
		reads_vlong(&a,"1");
		reads_vlong(&e,"1");
		div_long(&a,1,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111");
		reads_vlong(&e,"37");
		div_long(&a,3,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111");
		reads_vlong(&e,"3003003");
		div_long(&a,37,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	return 0;
}

int make_tests()
{
	equ_vlong_test();
	add_vlong_test();
	sub_vlong_test();
	mul_vlong_test();
	div_vlong_test();
	return 0;	
}
/* ------------------------------------------------------------------------------------------ */

/*
int if_in(char c,char* str)
{
	size_t length = strlen(str);
	int i = 0;
	while(i < length)
		if(str[i++] == c)return 1;
	return 0;
}

enum operation{plus=0,minus,divide,multiplex,bracket};
int  op_priority = {0,0,1,1,1};
char op_char[] = "+-/*()";

typedef struct {
	int val[MAXLEN];
	int sp;
}stack;

void push(stack *s,int x)
{
	s->val[s->sp++] = x;
}

int pop(stack *s)
{
	return s->val[--s->sp];
}

char 



typedef struct leaf_{
	leaf *pleft;
	leaf *pright;
	operation op;
}leaf;

leaf* tree_head = (leaf*)malloc(sizeof(leaf));
tree_head->op = bracket;
tree_head->pleft  = NULL;
tree_head->pright = NULL;


void parse_expression(char* str,leaf* tree_head)
{
	
}
*/

