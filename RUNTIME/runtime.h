/*
// A basic runtime for LAMBDA
*/

/* ****** ****** */
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* ****** ****** */
#define TAGnil 0
#define TAGint 1
#define TAGbtf 2
#define TAGstr 3
#define TAGtup 4
#define TAGlst 5
#define TAGlzy 6
#define TAGstrm 7
#define TAGcfp 8 // closure-function-pointer

/* ****** ****** */
extern void* mymalloc(size_t);

/* ****** ****** */
extern void* mymalloc(size_t n) {
    void* p0;
    p0 = malloc(n);
    if (p0 != 0) return p0;
    fprintf(stderr, "myalloc failed!!!\n");
    exit(1);
}

/* ****** ****** */
typedef struct{ 
    int tag; 
} lamval0;
typedef lamval0 *lamval1;

/* ****** ****** */
typedef struct{
    int tag; 
    int data;
} lamval0_int;
typedef lamval0_int *lamval1_int;

/* ****** ****** */
typedef struct{
    int tag;
    bool data;
} lamval0_btf;
typedef lamval0_btf *lamval1_btf;

/* ****** ****** */
typedef struct{
    int tag; 
    char *data;
} lamval0_str;
typedef lamval0_str *lamval1_str;

/* ****** ****** */
typedef struct{
    int tag; 
    lamval1 fst;
    lamval1 snd;
} lamval0_tup;
typedef lamval0_tup *lamval1_tup;

/* ****** ****** */
typedef struct{
    int tag; 
    lamval1 head;
    lamval1 tail;
} lamval0_lst;
typedef lamval0_lst *lamval1_lst;

/* ****** ****** */
typedef struct{
    int tag; 
    void* env;
    lamval1 lazy_cfp;
    lamval1 arg;
} lamval0_lzy;
typedef lamval0_lzy *lamval1_lzy;


/* ****** ****** */
typedef struct{
    int tag; 
    lamval1 head;
    lamval1 tail;
} lamval0_strm;
typedef lamval0_strm *lamval1_strm;

/* ****** ****** */
typedef struct{
    int tag; 
    void* env;
    lamval1 (*F)(lamval1, void*);
} lamval0_cfp;
typedef lamval0_cfp *lamval1_cfp;

/* ****** ****** */
int LAM_VAL_tag(lamval1 x){
    return x->tag;
}

/* ****** ****** */
extern lamval1 LAM_ERR();
extern lamval1 LAM_VAL_NIL();
extern lamval1 LAM_VAL_INT(int);
extern lamval1 LAM_VAL_BTF(bool);
extern lamval1 LAM_VAL_STR(char*);
extern lamval1 LAM_VAL_TUP(lamval1, lamval1);
extern lamval1 LAM_FST(lamval1);
extern lamval1 LAM_SND(lamval1);
extern lamval1 LAM_CAL(lamval1, lamval1);
extern lamval1 LAM_LAZY(lamval1, lamval1);
extern lamval1 LAM_CFP(lamval1 (*)(lamval1, void*), void*);
extern void BIND(lamval1, lamval1);

/* ****** ****** */
extern lamval1 LAM_ERR(){
    printf("Error");
    return NULL;
}

/* ****** ****** */
extern lamval1 LAM_VAL_NIL(){
    lamval1 p0;
    p0 = mymalloc(sizeof(lamval0));
    p0->tag = TAGnil;  
    
    return p0;
}

/* ****** ****** */
extern lamval1 LAM_VAL_INT(int i){
    lamval1_int p0;
    p0 = mymalloc(sizeof(lamval0_int));
    p0->tag = TAGint; 
    p0->data = i; 

    return (lamval1)p0;
}

/* ****** ****** */
extern lamval1 LAM_VAL_BTF(bool b){
    lamval1_btf p0;
    p0 = mymalloc(sizeof(lamval0_btf));
    p0->tag = TAGbtf; 
    p0->data = b; 

    return (lamval1)p0;
}

/* ****** ****** */
extern lamval1 LAM_VAL_STR(char* c){
    lamval1_str p0;
    p0 = mymalloc(sizeof(lamval0_str));
    p0->tag = TAGstr; 
    p0->data = c; 

    return (lamval1)p0;
}

/* ****** ****** */
extern lamval1 LAM_VAL_TUP(lamval1 fst, lamval1 snd){
    lamval1_tup p0;
    p0 = mymalloc(sizeof(lamval0_tup));
    p0->tag = TAGtup; 
    p0->fst = fst;
    p0->snd = snd; 

    return (lamval1)p0;
}

/* ****** ****** */
// TODO: remove
extern lamval1 LAM_VAL_FUN(lamval1 (*F)(lamval1, void*)){
    return LAM_CFP(F, NULL);
}

/* ****** ****** */
extern lamval1 LAM_FST(lamval1 tup){
    lamval1 fst = ((lamval1_tup)tup)->fst;
    return fst;
}

/* ****** ****** */
extern lamval1 LAM_SND(lamval1 tup){
    lamval1 snd = ((lamval1_tup)tup)->snd;
    return snd;
}

/* ****** ****** */
extern lamval1 LAM_CAL(lamval1 f, lamval1 x){
    return ((lamval1_cfp)f)->F(x, ((lamval1_cfp)f)->env);
}

/* ****** ****** */
extern lamval1 LAM_LAZY(lamval1 lazy_cfp, lamval1 x){
    lamval1_lzy p0;
    p0 = mymalloc(sizeof(lamval0_lzy));
    p0->tag = TAGlzy; 
    p0->lazy_cfp = lazy_cfp;
    p0->env = NULL; // is this correct? idk
    p0->arg = x;

    return (lamval1)p0;
}

/* ****** ****** */
extern lamval1 LAM_CFP(lamval1 (*F)(lamval1, void*), void* env){
    lamval1_cfp p0;
    p0 = mymalloc(sizeof(lamval0_cfp));
    p0->tag = TAGcfp;
    p0->env = env;
    p0->F = F;
    
    return (lamval1)p0;
}

/* ****** ****** */
extern lamval1 LAM_OPR_neg(lamval1);
extern lamval1 LAM_OPR_add(lamval1, lamval1);
extern lamval1 LAM_OPR_sub(lamval1, lamval1);
extern lamval1 LAM_OPR_mul(lamval1, lamval1);
extern lamval1 LAM_OPR_mod(lamval1, lamval1);
extern lamval1 LAM_OPR_ilt(lamval1, lamval1);
extern lamval1 LAM_OPR_igt(lamval1, lamval1);
extern lamval1 LAM_OPR_ieq(lamval1, lamval1);
extern lamval1 LAM_OPR_ige(lamval1, lamval1);
extern lamval1 LAM_OPR_ile(lamval1, lamval1);
extern lamval1 LAM_OPR_neq(lamval1, lamval1);
extern lamval1 LAM_OPR_list_nil();
extern lamval1 LAM_OPR_list_cons(lamval1, lamval1);
extern lamval1 LAM_OPR_list_nilq(lamval1);
extern lamval1 LAM_OPR_list_consq(lamval1);
extern lamval1 LAM_OPR_list_uncons1(lamval1);
extern lamval1 LAM_OPR_list_uncons2(lamval1);
extern lamval1 LAM_OPR_strm_eval(lamval1);
// extern lamval1 LAM_OPR_strm_lazy(lamval1);
extern lamval1 LAM_OPR_strm_nil();
extern lamval1 LAM_OPR_strm_cons(lamval1, lamval1);
extern lamval1 LAM_OPR_strm_nilq(lamval1);
extern lamval1 LAM_OPR_strm_consq(lamval1);
extern lamval1 LAM_OPR_strm_uncons1(lamval1);
extern lamval1 LAM_OPR_strm_uncons2(lamval1);
extern lamval1 LAM_OPR_show(lamval1);
extern lamval1 LAM_OPR_print(lamval1);
extern lamval1 LAM_OPR_showval(lamval1);
extern void print_lst();

/* ****** ****** */
extern lamval1 LAM_OPR_neg(lamval1 x){
    return LAM_VAL_INT(-1 * ((lamval1_int)x)->data);
}

extern lamval1 LAM_OPR_add(lamval1 x, lamval1 y){
    return LAM_VAL_INT(((lamval1_int)x)->data + ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_sub(lamval1 x, lamval1 y){
    return LAM_VAL_INT(((lamval1_int)x)->data - ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_mul(lamval1 x, lamval1 y){
    return LAM_VAL_INT(((lamval1_int)x)->data * ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_mod(lamval1 x, lamval1 y){
    return LAM_VAL_INT(((lamval1_int)x)->data % ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_ilt(lamval1 x, lamval1 y){
    return LAM_VAL_BTF(((lamval1_int)x)->data < ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_ile(lamval1 x, lamval1 y){
    return LAM_VAL_BTF(((lamval1_int)x)->data <= ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_igt(lamval1 x, lamval1 y){
    return LAM_VAL_BTF(((lamval1_int)x)->data > ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_ige(lamval1 x, lamval1 y){
    return LAM_VAL_BTF(((lamval1_int)x)->data >= ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_ieq(lamval1 x, lamval1 y){
    return LAM_VAL_BTF(((lamval1_int)x)->data == ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_neq(lamval1 x, lamval1 y){
    return LAM_VAL_BTF(((lamval1_int)x)->data != ((lamval1_int)y)->data);
}

extern lamval1 LAM_OPR_list_nil(){
    lamval1_lst p0;
    p0 = mymalloc(sizeof(lamval0_lst));
    p0->tag = TAGlst;  

    return (lamval1)p0;
} // end of [LAM_OPR_list_nil]

extern lamval1 LAM_OPR_list_cons(lamval1 x, lamval1 y){
    lamval1_lst p0;
    p0 = mymalloc(sizeof(lamval0_lst));
    p0->tag = TAGlst;  
    p0->head = x;
    p0->tail = y;

    return (lamval1)p0;
} // end of [LAM_OPR_list_cons]

extern lamval1 LAM_OPR_list_nilq(lamval1 x){
    return LAM_VAL_BTF(((lamval1_lst)x)->head == NULL);
}

extern lamval1 LAM_OPR_list_consq(lamval1 x){
    return LAM_VAL_BTF(((lamval1_lst)x)->head != NULL);
}

extern lamval1 LAM_OPR_list_uncons1(lamval1 x){
    return ((lamval1_lst)x)->head;
}

extern lamval1 LAM_OPR_list_uncons2(lamval1 x){
    return ((lamval1_lst)x)->tail;
}

extern lamval1 LAM_OPR_strm_eval(lamval1 x){
    return LAM_CAL(((lamval1_lzy)x)->lazy_cfp, (((lamval1_lzy)x)->arg));
}

extern lamval1 LAM_OPR_strm_nil(lamval1 x){
    lamval1_strm p0;
    p0 = mymalloc(sizeof(lamval0_strm));
    p0->tag = TAGstrm;  
    p0->tail = x;

    return (lamval1)p0;
} // end of [LAM_OPR_strm_nil]

extern lamval1 LAM_OPR_strm_cons(lamval1 x, lamval1 y){
    lamval1_strm p0;
    p0 = mymalloc(sizeof(lamval0_strm));
    p0->tag = TAGstrm;  
    p0->head = x;
    p0->tail = y;

    return (lamval1)p0;
} // end of [LAM_OPR_strm_cons]

extern lamval1 LAM_OPR_strm_nilq(lamval1 x){
    return LAM_VAL_BTF(((lamval1_strm)x)->head == NULL);
}

extern lamval1 LAM_OPR_strm_consq(lamval1 x){
    return LAM_VAL_BTF(((lamval1_strm)x)->head != NULL);
}

extern lamval1 LAM_OPR_strm_uncons1(lamval1 x){
    return ((lamval1_strm)x)->head;
}

extern lamval1 LAM_OPR_strm_uncons2(lamval1 x){
    return ((lamval1_strm)x)->tail;
}

extern lamval1 LAM_OPR_show(lamval1 x){
    return LAM_OPR_print(x);
}

extern lamval1 LAM_OPR_print(lamval1 x){
    if (x == NULL){
        printf("nullptr error\n");
        exit(1);
    }
    int tag = x->tag;
    switch(tag){
        case TAGnil:
            printf("()");
            break;
        case TAGint:
            printf("%i", ((lamval1_int)x)->data); 
            break;
        case TAGbtf:
            (((lamval1_btf)x)->data) ? printf("true") : printf("false");
            break;
        case TAGstr:
            printf("%s", ((lamval1_str)x)->data); 
            break;
        case TAGtup:
            printf("(");
            LAM_OPR_print((lamval1)(((lamval1_tup)x)->fst)); 
            printf(", ");
            LAM_OPR_print((lamval1)(((lamval1_tup)x)->snd));
            printf(")"); 
            break;
        case TAGlst:
            print_lst(x); 
            break;
        case TAGlzy:
            printf("<lamval1_lzy>");
            break;
        case TAGstrm:
            printf("<lamval1_strm>");
            break;
        case TAGcfp:
            printf("<lamval1_cfp>"); 
            break;
        default: 
            printf("Unrecognized tag = %i", tag);
    }
    return NULL;
} // end of [LAM_OPR_print]

extern void print_lst(lamval1 x){
    lamval1_lst lst = (lamval1_lst)x;
    if (!lst->head){
        printf("nil()");
    }
    else if (!lst->tail){
        printf("cons(");
        LAM_OPR_print((lamval1)(lst->head));
        printf(", nil())");
    }
    else{
        printf("cons(");
        LAM_OPR_print((lamval1)(lst->head)); 
        printf(", ");
        LAM_OPR_print((lamval1)(lst->tail)); 
        printf(")");
    }
} // end of [print_lst]

extern lamval1 LAM_OPR_showval(lamval1 x){
    return LAM_OPR_print(x);
}

/* ****** ****** */

/* end of [runtime.h] */
