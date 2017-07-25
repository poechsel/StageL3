#include <stdio.h>
#include <string.h>
#include <limits.h>

#define N 1000


struct s_iterator {
    int min;
    int max;
};
struct s_bounds {
    int *min;
    int *max;
};

struct s_infos {

    struct s_bounds f_r;
    struct s_bounds f_w;
    struct s_bounds f_rw;
};

struct s_array {
    int * f_r;
    int * f_w;
    int * f_rw;
};

int min(int a, int b) {
    return (a < b) ? a : b;
}
int max(int a, int b) {
    return (a > b) ? a : b;
}

int min3(int a, int b, int c) {
    return min(a, min(b, c));
}
int max3(int a, int b, int c) {
    return max(a, max(b, c));
}

struct s_iterator INTER_INTERVAL(struct s_iterator a, struct s_iterator b) {
    struct s_iterator out;
    out.min = max(a.min, b.min);
    out.max = min(a.max, b.max);
    printf("intersection [%d; %d] and [%d; %d] => [%d; %d]\n", a.min, a.max, b.min, b.max, out.min, out.max);
    return out;
}
struct s_iterator UNION_INTERVAL(struct s_iterator a, struct s_iterator b) {
    struct s_iterator out;
    out.min = min(a.min, b.min);
    out.max = max(a.max, b.max);
    return out;
}

struct s_iterator MAKE_OP_INTERVAL (char* op, int min_value, int max_value, int minus_div) {
    int d = -minus_div;
    struct s_iterator out;
    out.min = INT_MIN;
    out.max = INT_MAX;
    if (strncmp("==", op, 2) == 0) {
        out.min = min(min_value/d, max_value/d);
        out.max = max(min_value/d, max_value/d);
    } else if (strncmp(">", op, 1) == 0) {
        if (d > 0) {
            out.min = min(min_value / d, max_value / d) + 1;
        } else if (d < 0) {
            out.max = max(min_value / d, max_value / d);
        }
    } else if (strncmp("<=", op, 2) == 0) {
        if (d > 0) {
            out.max = max(min_value / d, max_value / d);
        } else if (d < 0) {
            out.min = min(min_value / d, max_value / d) + 1;
        }
    }

    return out;
}


int main(void) {
    int a[N];
    int b[N];

    for (int i = 0; i < N; ++i) {
        a[i] = 2;
        b[i] = 2;
    }



/*
    struct s_iterator  it_list[4];
    it_list[2] = UNION_INTERVAL(MAKE_OP_INTERVAL("==", 0, 0, -1), MAKE_OP_INTERVAL("==", 1*-1+N, 1*-1+N, -1));
    it_list[2] = INTER_INTERVAL(it_list[2], MAKE_OP_INTERVAL(">", 100, 100, -1));
    it_list[3] = UNION_INTERVAL(MAKE_OP_INTERVAL("==", 0, 0, -1), MAKE_OP_INTERVAL("==", 1*-1+N, 1*-1+N, -1));
    it_list[3] = INTER_INTERVAL(it_list[3], MAKE_OP_INTERVAL("<=", 100, 100, -1));
    struct s_infos  s_b_infos;
    s_b_infos.f_rw.min = (int *) malloc(sizeof(int)*1);
    s_b_infos.f_rw.max = (int *) malloc(sizeof(int)*1);
    s_b_infos.f_r.min = (int *) malloc(sizeof(int)*1);
    s_b_infos.f_r.max = (int *) malloc(sizeof(int)*1);
    s_b_infos.f_w.min = (int *) malloc(sizeof(int)*1);
    s_b_infos.f_w.max = (int *) malloc(sizeof(int)*1);
    int* s_b_f_w = b;
    struct s_infos  s_a_infos;
    s_a_infos.f_rw.min = (int *) malloc(sizeof(int)*1);
    s_a_infos.f_rw.max = (int *) malloc(sizeof(int)*1);
    s_a_infos.f_r.min = (int *) malloc(sizeof(int)*1);
    s_a_infos.f_r.max = (int *) malloc(sizeof(int)*1);
    s_a_infos.f_w.min = (int *) malloc(sizeof(int)*1);
    s_a_infos.f_w.max = (int *) malloc(sizeof(int)*1);
    int *s_a_f_w = a;

    {
        int  __a = min(1*it_list[3].min, 1*it_list[3].max);
        int  __b = max(1*it_list[3].min, 1*it_list[3].max);
        s_a_infos.f_w.min[0] = min(__a, __b);
        s_a_infos.f_w.max[0] = max(__a, __b);
    }


    {
        int  __a = min(1*it_list[2].min, 1*it_list[2].max);
        int  __b = max(1*it_list[2].min, 1*it_list[2].max);
        s_b_infos.f_w.min[0] = min(__a, __b);
        s_b_infos.f_w.max[0] = max(__a, __b);
    }

    printf("Iterators for a : [%d ; %d]\n", it_list[3].min, it_list[3].max);
    printf("Iterators for b : [%d ; %d]\n", it_list[2].min, it_list[2].max);


    {
#pragma acc data pcopyout(s_a_f_w[s_a_infos.f_w.min[0]:s_a_infos.f_w.max[0]-s_a_infos.f_w.min[0]]+1)

        {
#pragma acc data pcopyout(s_b_f_w[s_b_infos.f_w.min[0]:s_b_infos.f_w.max[0]-s_b_infos.f_w.min[0]+1])

            {
#pragma acc parallel loop
                for (int  i = 0; i<N; ++ i)
                {
                    if (i>100)s_b_f_w[i] = 0;     else s_a_f_w[i] = 1;
                }

            }

        }

    }
//*/
/*
    for (int i = 0; i<N; ++ i)
    {
     if (i>100)
         b[i] = 0;     
     else 
         a[i] = 1;
    }
    */

    
    for (int i = 0; i < N; ++i) {
        printf("%d ", a[i]);
    }
    printf("\n\n");
    for (int i = 0; i < N; ++i) {
        printf("%d ", b[i]);
    }
    printf("\n");
}
