#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAXLEN 1000
#define MAXPRIORITY 10
#define SYS 10000
#define LOGSYS 4
#define BUFFLEN 128
#define MINLEVEL 128

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

typedef struct {
	int val[MAXLEN];
	int sp;
}stack;


typedef enum {none = 0,plus,minus,divide,multiplex} operation;

typedef struct node{
	struct node *pleft;
	struct node *pright;
	struct node *parent;
	buffer *expr;
	operation op;
}node;

void add(vlong *op1, vlong *op2, vlong *res);
void sub(vlong *op1,vlong *op2,vlong *res);
void mul(vlong *op1, int sh,int offs, vlong *res);
void mul_long(vlong *op1, vlong *op2, vlong *res);
void div_short(vlong *op1,int sh, vlong *res);

vlong ltovlong(long long a);

int  cpy(vlong* src,vlong* ind);
int  geq(vlong* op1,vlong* op2);
int  leq(vlong* op1,vlong* op2);
int  les(vlong* op1,vlong* op2);
int  grt(vlong* op1,vlong* op2);
int  equ(vlong* op1,vlong* op2);

void print_vlong(vlong *c);
int  reads_vlong(vlong *c, char* str);
int  read_vlong(vlong *c);

void copy_buff(buffer* source,buffer* target,int st,int end);
int check_buff(buffer* r, char* template);
void filter_buff(buffer* r, char* filter);
void resize_buff(buffer* r,size_t n);
int  add_to_buff(buffer* r, char* s);
void init_buff(buffer* r,size_t n);
void normalize_buff(buffer* r);
void delete_buff(buffer* r);

int in_str(char c,char* str);

int assert_expr(buffer* r);
int build_lex_tree(buffer* r);
int is_unary_op(buffer* r, int i);

void init_stack(stack* s);
void push(stack *s,int x);
int pop(stack *s);

node* new_node(node* parent);
int  delete_node(node* r);
int delete_tree(node* parent);
int eval_tree(node* parent,vlong* res);
void print_tree(node* parent,int level);
int buff_to_vlong(buffer* r,vlong* res);

int make_tests();
int equ_vlong_test();
int add_vlong_test();
int sub_vlong_test();
int mul_vlong_test();
int div_vlong_test();
int build_tree_test();

int filter_buff_test();
int buff_test();
int expr_test();


int main(void)
{
	//make_tests();
	build_tree_test();
	return 0;
}

/* ------------------------------------------------------------------------------------------ */
/* -------------------------------------------Parser----------------------------------------- */

int build_tree(buffer* r,node* parent)
{
	int  op_priority[] = {1,0,0,1,1};
	char* op  = "~+-/*";
	operation mnop;

	int s = 0;

	int mnop_ptr = 0;
	int level = 0;
	
	int i = 0;
	int temp = 0;

	int maxlevel = 0;
	int minlevel = MINLEVEL;

	/* The goal is to find min priority term on current level and put is as node of the tree */

	for(i = 0; i < r->pos;i++)
	{
		if(r->buf[i] == '(') {s += 1;continue;}
		if(r->buf[i] == ')') {s -= 1;continue;}
		
		//printf("s: %d\n",s);

		if(maxlevel < s) maxlevel = s;
		if(minlevel > s) minlevel = s;
	}
	level = minlevel;
	
	/*TODO Initialize variable that are reused by the cycle*/
	mnop = none;
	mnop_ptr = 0;
	temp = 0;
	s = 0; /*Clear the stack TODO Use counter instead of stack*/

	/* Find the maxlevel of brackets*/
	for(i = 0; i < r->pos;i++)
	{
		if(r->buf[i] == '('){s += 1;continue;}
		if(r->buf[i] == ')'){s -= 1;continue;}
	
		if(level == s){
			temp = in_str(r->buf[i],op);
			if(temp > 0)
			{
				if(op_priority[temp] <= op_priority[mnop])
				{
					mnop = (operation)temp;
					mnop_ptr = i;
				}
			}
		}
			
	}
	
	if(mnop_ptr == 0)
	{
		return 0;
	}
		
	parent->op = mnop;
	parent->expr = r;
	parent->pleft = new_node(parent);
	parent->pright = new_node(parent);
	
	copy_buff(parent->expr,parent->pleft->expr,0,mnop_ptr-1);
	copy_buff(parent->expr,parent->pright->expr,mnop_ptr+1,parent->expr->pos-1);

	build_tree(parent->pleft->expr,parent->pleft);
	build_tree(parent->pright->expr,parent->pright);
	return 0;
	
}
void print_tree(node* parent,int level)
{
	printf("level: %d\n",level);
	printf("operation: %d\n",(int)parent->op);
	printf("buff: %s\n\n",parent->expr->buf);
	if(parent->pleft != NULL)  {print_tree(parent->pleft,level+1);}
	if(parent->pright != NULL) {print_tree(parent->pright,level+1);}

	return;
}

int is_leaf(node* leaf)   
{
	if(leaf == NULL)return -1;
	if(leaf->pleft == NULL &&  leaf->pright == NULL)
	{
		if(leaf->parent != NULL)return 1;
	}
	return 0;
}

int eval_tree(node* parent,vlong* res)
{
	if(is_leaf(parent))
	{
		printf("LEAF!!\n");
		filter_buff(parent->expr,"()"); 
		if(check_buff(parent->expr,"@1234567890") != 1)return -1; 
		if(buff_to_vlong(parent->expr,res) != 1) return -1; 
		return 0;
	}
	
	vlong op1,op2;
	op1.val[MAXLEN] = 0;
	op2.val[MAXLEN] = 0;
	op1.st = MAXLEN;
	op2.st = MAXLEN;

	if(eval_tree(parent->pleft, &op1) != 0)return -1;
	if(eval_tree(parent->pright,&op2) != 0)return -1;
	printf("op1 : ");
	print_vlong(&op1);
	printf("\n");
	printf("op2 : ");
	print_vlong(&op2);
	printf("\n--------------------\n");
	
	if(parent->op == none)return -1;
	printf("Choose op\n");
	printf("Parent op : %d\n",parent->op);

	switch(parent->op)
	{
		case plus:
		{
			printf("plus\n");
			add(&op1,&op2,res);
			break;
		}
		case minus:
		{
			printf("minus\n");
			sub(&op1,&op2,res);
			break;
		}
		case divide:
		{
			printf("div\n");
			div_short(&op1,1,res); //TODO Fix this
			break;
		}
		case multiplex:
		{
			printf("mul\n");
			mul_long(&op1,&op2,res);
			break;
		}

		default:
		{
			printf("default\n");
			return -1;
		}
	}
	printf("res : ");
	print_vlong(res);
	printf("\n--------------------\n");
	return 0;
}

int is_unary_op(buffer* r, int i)
{
	char* allowed_l = "+*/()1234567890";
	char* allowed_r = "(1234567890";
	int r_expr=0,l_expr=0;

	if(r->buf[i] != '-' )return 0;
	if(i < 0 || i >= r->pos-1)return -1;

	if(i == 0)return 1;
	l_expr = in_str(r->buf[i-1],allowed_l);
	r_expr = in_str(r->buf[i+1],allowed_r);

	if(l_expr < 0  || r_expr < 0)return -1;
	if(l_expr >= 4 || r_expr == 0)return 0; //here is the heck
	if(l_expr >= 0 && l_expr <= 3)return 1;

	return 0;	
}

/* ------------------------------------------------------------------------------------------ */
/* ---------------------------------------Long Arithmetics---------------------------------- */

void print_vlong(vlong *c)
{
	int i;
	printf("%d",c->val[c->st]);
	for(i = c->st + 1; i <= MAXLEN;i++)
		printf("%.4d",c->val[i]);
}

int  cpy(vlong* src,vlong* ind)
{
	int i = 0;
	int st  = src->st;
	ind->st = src->st;
	for(i = MAXLEN;i >= st;i--)
		ind->val[i] = src->val[i];

	return 0;
}

vlong ltovlong(long long a)
{
	vlong n;
	n.val[MAXLEN] = 0;
	n.st = MAXLEN;

	while(a != 0)
	{
		n.val[n.st] = a % SYS;
		a = a / SYS;
		n.st = n.st - 1;
	}
	n.st += 1;

	return n;
}

int read_vlong(vlong *c)
{
	char str[MAXLEN];
	scanf("%s",str);
	return reads_vlong(c,str);
}

int reads_vlong(vlong *c, char* str)
{
	int   st = MAXLEN, temp;
	int   is_negative = 0;
	char  r_val[LOGSYS+1];
	char* str_pos;
	char  tmp = '\0';

	if(str[0] == '-')
	{
		is_negative = 1;
		str = str + 1;
	}

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

	if(is_negative)
	{
		for(i = c->st;i <= MAXLEN;i++)
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

void div_short(vlong *op1,int sh, vlong *res)
{
	int i = MAXLEN;
	int st = op1->st;
	int rem = 0;
	long long a = 0;
	long long temp = 0;
	long long flag = 0;

	vlong res_tmp,a_long;
	res_tmp.val[MAXLEN] = 0;
	res_tmp.st = 0;

	res->val[MAXLEN] = 0;
	res->st = MAXLEN;

	for(i=st; i <= MAXLEN;i++)
	{
		cpy(res,&res_tmp);
		mul(&res_tmp,SYS,0,res);

		temp = op1->val[i] + flag;
		a   = (long long) temp / sh; // Here is the heck!
		rem = (long long) temp % sh;

		a_long = ltovlong(a);
		add(res,&a_long,res);

		flag = rem * SYS;
	}
}


/* ------------------------------------------------------------------------------------------ */

/*------------------------------------------Buffer--------------------------------------------*/
void resize_buff(buffer* r,size_t n)
{
	r->buf = (char*)realloc(r->buf,n*sizeof(char));
	memset((r->buf+r->pos),'\0',n - r->pos);
	r->pos = r->pos > n ? n : r->pos; /*TODO Make sure about this expr*/
	r->size = n;
}
void init_buff(buffer* r,size_t n)
{
	r->buf = (char*)malloc(n*sizeof(char));
	memset(r->buf,'\0',n);
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
void clear_buff(buffer* r)
{
	memset(r->buf,'\0',sizeof(char)*r->size);
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
		if(r->pos >= r->size - 1)
			resize_buff(r,2*r->size);

		if(s[i] != ' ' && s[i] != '\n')
		{
			r->buf[r->pos++] = s[i];
			count++;
		}
		else continue;
	}
	return count;	
}

void normalize_buff(buffer* r) /*Change all unary minuses to @ */
{
	int i = 0,temp = 0;
	char* op  = "~+-/*";
	char* minus_unary = "@";
	buffer* tmp = (buffer*)malloc(sizeof(buffer));
	init_buff(tmp,r->pos + 1);
	
	for(i = 0;i < r->pos;i++)
	{
		if(r->buf[i] == '-')
		{
			if(is_unary_op(r,i)){ add_to_buff(tmp,minus_unary); }			
			else { tmp->buf[tmp->pos++] = r->buf[i]; }
		}
		else{
			tmp->buf[tmp->pos++] = r->buf[i];
		}
	}
	copy_buff(tmp,r,0,tmp->pos-1);
	delete_buff(tmp);
	free(tmp);

	return;
}

void filter_buff(buffer* r, char* filter)
{	
	int i = 0,temp = 0;
	buffer* tmp = (buffer*)malloc(sizeof(buffer));
	init_buff(tmp,r->pos + 1);
	for(i = 0;i < r->pos;i++)
	{
		if(in_str(r->buf[i],filter) < 0)tmp->buf[tmp->pos++] = r->buf[i];
		else continue;
	}

	copy_buff(tmp,r,0,tmp->pos-1);
	delete_buff(tmp);
	free(tmp);
	return;
}

int check_buff(buffer* r, char* template)
{
	int i = 0;
	for(i = 0;i < r->pos;i++)
	{
		if(in_str(r->buf[i],template) < 0) return 0;
	}

	return 1;
}

int buff_to_vlong(buffer* r,vlong* res)
{
	int i = 0,temp = 0;
	res->val[MAXLEN] = 0;
	res->st = 0;

	if(check_buff(r,"@1234567890") != 1)return 0;
	if(r->buf[0] == '@')r->buf[0] = '-';

	reads_vlong(res,r->buf);

	return 1;
}

void copy_buff(buffer* source,buffer* target,int st,int end)
{
	int size = end - st;
	int i = 0;

	if(size < 0 || size > source->pos+1)return;
	clear_buff(target);

	if(target->size < size)
		resize_buff(target,size+1);

	for(i = 0;i <= size;i++) /*Less or Leq*/
		target->buf[i] = source->buf[st+i];
	
	target->pos = size+1;
}

/* ------------------------------------------------------------------------------------------ */
/* ------------------------------------------Stack------------------------------------------- */

void push(stack *s,int x)
{
	s->val[s->sp++] = x;
}

int pop(stack *s)
{
	return s->val[--s->sp];
}

void init_stack(stack* s)
{
	memset(s->val,0,sizeof(int)*MAXLEN);
	s->sp = 0;
}
/* ------------------------------------------------------------------------------------------ */
/* ------------------------------------------Node-------------------------------------------- */

node* new_node(node* parent)
{
	node* r = (node*)malloc(sizeof(node));
	r->expr = (buffer*)malloc(sizeof(buffer));

	r->pleft = NULL;
	r->pright = NULL;
	r->parent = parent;
	r->op = none;
	init_buff(r->expr,BUFFLEN);
	
	return r;
}

int  delete_node(node* r)
{
	int status = 0;
	if(r == NULL) return status;	

	if(r->pright != NULL) status += 1;
	if(r->pleft != NULL) status += 2;

	if(r->expr != NULL){
		delete_buff(r->expr);
		free(r->expr);
		r->expr = NULL;
	}

	if(r->parent != NULL) {
		if(r == r->parent->pleft)
			r->parent->pleft = (node*)NULL;
		else if(r == r->parent->pright)
			r->parent->pright = NULL;
		else status += 4;
	}    
	free(r);
	r = NULL;

	return status;	
}

int delete_tree(node* parent)
{
	int status = 0;
	if(parent->pleft != NULL)
		status += delete_tree(parent->pleft);
	if(parent->pright != NULL)
		status += delete_tree(parent->pright);

	status += delete_node(parent);
	return status;
}


/* ------------------------------------------------------------------------------------------ */
/* -------------------------------------Check expressions------------------------------------ */
int 	in_str(char c,char* str)
{
	size_t length = strlen(str);
	int i = 0;
	while(i < length)
	{
		if(str[i] == c)return i;
		i += 1;
	}

	return -1;
}

int assert_expr(buffer* r)
{
	char* allowed_symbols = "1234567890*-+/()";
	stack s;
	init_stack(&s);

	int i = 0;
	for(i = 0;i < r->pos;i++)
	{
		if(in_str(r->buf[i], allowed_symbols) != -1)
		{
			if(r->buf[i] == '('){push(&s,1);continue;}
			if(r->buf[i] == ')'){
				if(s.sp > 0){
					pop(&s);	
				}
				else return -1;
			}
			
		}
		else return -1;
	}
	if(s.sp != 0) return -1;

	return 0;
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
int div_short_test()
{
	vlong a,b,c,e;
	size_t test_num = 0;

	log("Test %zu",test_num++);
		reads_vlong(&a,"1");
		reads_vlong(&e,"1");
		div_short(&a,1,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111");
		reads_vlong(&e,"37");
		div_short(&a,3,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111");
		reads_vlong(&e,"3003003");
		div_short(&a,37,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"111111111");
		reads_vlong(&e,"-3003003");
		div_short(&a,-37,&c);
	if(!equ(&e,&c))printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	return 0;
}
void ltovlong_test()
{
	vlong a,b,c,e;
	long long   r;
	size_t test_num = 0;
	log("Test %zu",test_num++);
		reads_vlong(&a,"999999999999");
		b = ltovlong(999999999999);
	if(!equ(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
		reads_vlong(&a,"-99999999");
		b = ltovlong(-99999999);
	if(!equ(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");
}

int cpy_vlong_test()
{
	vlong a,b,c,e;
	size_t test_num = 0;

	log("Test %zu",test_num++);
		reads_vlong(&a,"1234567890098765457635428635");
		reads_vlong(&b,"13124234535625452435234525245234523542");
		cpy(&a,&b);
	if(!equ(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	
	return 0;
}

int buff_test()
{
	size_t test_num = 0;
	char* test = "      123456         + \n \n 11111111 \n-\n193/ 2\n";
	char* res  = "123456+11111111-193/2";

	log("Test %zu",test_num++);
		buffer r;
		init_buff(&r,2);
        	add_to_buff(&r, test);
	if(strcmp(r.buf,res) != 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");

	delete_buff(&r);
}

int copy_buff_test()
{
	size_t test_num = 0;
	buffer *b1,*b2;

	b1 = (buffer*)malloc(sizeof(buffer));
	b2 = (buffer*)malloc(sizeof(buffer));

	init_buff(b1,2);
	init_buff(b2,2);

	log("Test %zu",test_num++);
		add_to_buff(b1,"(1+3)*2+1");
		copy_buff(b1,b2,0,7-1);	
	if(strcmp(b2->buf,"(1+3)*2") != 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");

	clear_buff(b1);
	log("Test %zu",test_num++);
		add_to_buff(b1,"(1+3)*2+1");
		copy_buff(b1,b2,7+1,9);	
	if(strcmp(b2->buf,"1") != 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");
	delete_buff(b1);
	delete_buff(b2);
	
	free(b1);
	free(b2);
}

int normalize_buff_test()
{
	size_t test_num = 0;
	buffer *b1 = (buffer*)malloc(sizeof(buffer));
	init_buff(b1,128);
	log("Test %zu",test_num++);
		add_to_buff(b1,"(-128+392)*2-10/-5");
		normalize_buff(b1);
	if(strcmp(b1->buf,"(@128+392)*2-10/@5") != 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");
	delete_buff(b1);;
	free(b1);

	return 0;
}
int is_leaf_test()
{
	size_t test_num = 0;

	node* p = new_node(NULL);
	p->pleft = new_node(p);
	p->pright = new_node(p);

	log("Test %zu",test_num++);
	if(is_leaf(p))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
	if(!is_leaf(p->pleft))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	log("Test %zu",test_num++);
	if(!is_leaf(p->pright))printf("...FAIL\n");
	else printf("...SUCCESS\n");
	delete_tree(p);

	return 0;
}


int filter_buff_test() 
{ 
	size_t test_num = 0;
	buffer *b1 = (buffer*)malloc(sizeof(buffer));
	init_buff(b1,128);

	log("Test %zu",test_num++);
		add_to_buff(b1,"(@14721213");
		filter_buff(b1,"()");
	if(strcmp(b1->buf,"@14721213") != 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");
	clear_buff(b1);

	log("Test %zu",test_num++);
		add_to_buff(b1,"(23");
		filter_buff(b1,"()");
	if(strcmp(b1->buf,"23") != 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");

	delete_buff(b1);
	free(b1);
	
	return 0;
}

int check_buff_test() 
{
	size_t test_num = 0;
	buffer *b1 = (buffer*)malloc(sizeof(buffer));
	init_buff(b1,128);

	log("Test %zu",test_num++);
		add_to_buff(b1,"@147223423987345273952741213");
	if(check_buff(b1,"@1234567890") != 1)printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	clear_buff(b1);

	log("Test %zu",test_num++);
		add_to_buff(b1,"@1472234239873452+73952741213)()");
	if(check_buff(b1,"@1234567890") != 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	clear_buff(b1);

	log("Test %zu",test_num++);
		add_to_buff(b1,"@1472234239873452+73952741213");
	if(check_buff(b1,"@1234567890") != 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");

	delete_buff(b1);
	free(b1);
	
	return 0;
}

int buff_to_vlong_test()
{
	size_t test_num = 0;
	vlong a,b;
	char *vl1 = "@173749238542";
	char *vl2 = "23";
	buffer *b1 = (buffer*)malloc(sizeof(buffer));

	init_buff(b1,128);
	log("Test %zu",test_num++);
		add_to_buff(b1,vl1);
		buff_to_vlong(b1,&a);
		reads_vlong(&b,"-173749238542");
	if(!equ(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");

	clear_buff(b1);

	a.val[MAXLEN] = 0;
	a.st = MAXLEN;

	log("Test %zu",test_num++);
		add_to_buff(b1,vl2);
		buff_to_vlong(b1,&a);
		reads_vlong(&b,"23");
	if(!equ(&a,&b))printf("...FAIL\n");
	else printf("...SUCCESS\n");
	delete_buff(b1);
	free(b1);
		
	return 0;
}

int expr_test()
{
	size_t test_num = 0;
	char* bad_expr_1 = " (1 +232482739482)*)12312";
	char* bad_expr_2 = " (1 +232482739482)*123adajsd12";
	char* good_expr_1  = "(101231 + 1231 213) + 21312\n1231";

	buffer r;
	init_buff(&r,64);

	log("Test %zu",test_num++);
		add_to_buff(&r,bad_expr_1);
	if(assert_expr(&r) == 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");
	
	clear_buff(&r);
	log("Test %zu",test_num++);
		add_to_buff(&r,bad_expr_2);
	if(assert_expr(&r) == 0)printf("...FAIL\n");
	else printf("...SUCCESS\n");

	clear_buff(&r);
	log("Test %zu",test_num++);
		add_to_buff(&r,good_expr_1);
	if(assert_expr(&r) == -1)printf("...FAIL\n");
	else printf("...SUCCESS\n");
	delete_buff(&r);
}

int build_tree_test()
{
	size_t test_num = 0;
	
	char *expr = "-5*((-7)) + (23+7)-10";
	node* parent = new_node(NULL);

	add_to_buff(parent->expr,expr);
	printf("pos: %d\n",parent->expr->pos);
	printf("Is valid: %d\n",assert_expr(parent->expr));
	printf("pos after assert: %d\n",parent->expr->pos);
	normalize_buff(parent->expr);
	printf("pos after normalize: %d\n",parent->expr->pos);
	log("Test %zu",test_num++);

	build_tree(parent->expr,parent);
	print_tree(parent,0);

	vlong res;
	res.val[MAXLEN] = 0;
	res.st = MAXLEN;
	eval_tree(parent,&res);

	printf("res: ");
	print_vlong(&res);
	printf("\n");

	print_tree(parent,0);
	delete_tree(parent);
	return 0;
}

int make_tests()
{
	equ_vlong_test();
	add_vlong_test();
	sub_vlong_test();
	mul_vlong_test();
	div_short_test();
	ltovlong_test();
	cpy_vlong_test();
	buff_test();
	expr_test();
	copy_buff_test();
	normalize_buff_test();
	filter_buff_test();

	is_leaf_test();
	check_buff_test();
	buff_to_vlong_test();

	return 0;	
}
/* ------------------------------------------------------------------------------------------ */

