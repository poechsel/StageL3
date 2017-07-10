#include <stdio.h>
#include <stdlib.h>

int min(int a, int b) {
    return (a > b) ? b: a;
}

int max(int a, int b) {
    return (a > b) ? a : b;
}

typedef struct {
    int min;
    int max;
} s_iterators;

typedef struct {
    int *min;
    int *max;
} s_b;

typedef struct {
    s_b f_w;
    s_b f_r;
    s_b f_rw;
} s_bounds;

typedef struct {
    int *f_w;
    int *f_r;
    int *f_rw;
} s_attributes;


int gcd(int a, int b) {
    if (b == 0)
        return a;
    return gcd(b, a%b);
}
int lcm(int a, int b) {
    return a * b / gcd(a, b);
}


int intersection_interval(
        int m1, int M1, int a1, int b1,
        int m2, int M2, int a2, int b2
        ) {
    int i1 = (a1 > 0)? (int)((m1-b1)/a1)-1 : (int)(m1-b1)/a1+1;
    int i2 = (a2 > 0)? (int)((m2-b2)/a2)-1 : (int)(m2-b2)/a2+1;
    i1 = a1 * i1 + b1;
    i2 = a2 * i2 + b2;
    printf("%d %d\n", i1, i2);
    int found = 0;
    for (;i1<=M1;) {
        i1 += a1;
        if (m2 <= i1 && i1 <= M2) {
            found = 1;
            break;
        }
    } 
    if (!found) return 0;
    for (;i2<=M2;) {
        i2 += a2;
        if (m1 <= i2 && i2 <= M1) {
            found = 1;
            break;
        }
    } 
    if (!found) return 0;

    for (; i1 <= M1 && i2 <= M2;) {
        if (i1 == i2)
            return 1;
        else if (i1 < i2)
            i1 += a1;
        else 
            i2 += a2;
    }
    return 0;
}


int main(void) {

    int n = 10000;
    int m = n+18;
    int a[3*n];
    for (int i  = 0; i < n; ++i) {
        a[i] = 1;
        a[i+n] = 0;
    }


    /* begin generated code */
    s_iterators it_list[1];
    it_list[0].min = 0;
    it_list[0].max = 1 * -1+n;


    s_bounds a_infos;
    a_infos.f_w.min = (int*)malloc(sizeof(int) * 1);
    a_infos.f_w.max = (int*)malloc(sizeof(int) * 1);
    a_infos.f_rw.min = (int*)malloc(sizeof(int) * 1);
    a_infos.f_rw.max = (int*)malloc(sizeof(int) * 1);
    a_infos.f_r.min = (int*)malloc(sizeof(int) * 1);
    a_infos.f_r.max = (int*)malloc(sizeof(int) * 1);
    {
        int ___a = min(1*it_list[0].min, 1*it_list[0].max) + m;
        int ___b = max(1*it_list[0].min, 1*it_list[0].max) + m;
        a_infos.f_w.min[0] = min(___a, ___b);
        a_infos.f_w.max[0] = max(___a, ___b);
    }
    {
        int ___a = min(1*it_list[0].min, 1*it_list[0].max);
        int ___b = max(1*it_list[0].min, 1*it_list[0].max);
        a_infos.f_r.min[0] = min(___a, ___b);
        a_infos.f_r.max[0] = max(___a, ___b);
    }

    printf("interval for a_infos f_w: %d; %d\n", a_infos.f_w.min[0], a_infos.f_w.max[0]);
    printf("interval for a_infos f_r: %d; %d\n", a_infos.f_r.min[0], a_infos.f_r.max[0]);



    s_attributes s_a;
    s_a.f_w = a;
    s_a.f_r = a;
    s_a.f_rw = a;
    /* if their is an intersection between read and write parts */
    int inter_1 = 0;
    if ( /*(m - 0) % gcd(1, 1) == 0 
        &&*/ intersection_interval(a_infos.f_w.min[0], a_infos.f_w.max[0], 1, m,
            a_infos.f_r.min[0], a_infos.f_r.max[0], 1, 0)){

        printf("intersection\n");
        
        {
            a_infos.f_rw.min[0] = min(a_infos.f_r.min[0], a_infos.f_w.min[0]);
            a_infos.f_rw.max[0] = max(a_infos.f_r.max[0], a_infos.f_w.max[0]);
            printf("%d <-> %d\n", a_infos.f_rw.min[0], a_infos.f_rw.max[0]);
            int __a = a_infos.f_rw.min[0];
            int __b = a_infos.f_rw.max[0];
            inter_1 = 1;
#pragma acc data copy(s_a) copy(s_a.f_rw[__a : __b]) 
            {
#pragma acc kernels 
                for (int  i = 0; (i<n); i ++) 
                {
                    s_a.f_rw [(i+m)] = (2*s_a.f_rw [i]);
                }
            }
        }
        
    } else {
        printf("no intersection\n");
            inter_1 = 1;
            s_a.f_w = a + a_infos.f_w.min[0];
            a_infos.f_w.max[0] = a_infos.f_w.max[0] - a_infos.f_w.min[0];
            a_infos.f_w.min[0] = 0;
            printf("%d <-> %d\n", a_infos.f_w.min[0], a_infos.f_w.max[0]);
            printf("%d <-> %d\n", a_infos.f_r.min[0], a_infos.f_r.max[0]);

#pragma acc data copy(s_a) copyout(s_a.f_w[a_infos.f_w.min[0] : a_infos.f_w.max[0] - a_infos.f_w.min[0]]) copyin(s_a.f_r[a_infos.f_r.min[0] : a_infos.f_r.max[0]- a_infos.f_r.min[0]]) if (inter_1)
            {
#pragma acc kernels
                for (int  i = 0; (i<n); i ++) 
                {
                    s_a.f_w [(i)] = (2*s_a.f_r [i]);
                }
            }
    }

    /* end generated code */


    for (int i  = 0; i < 2*n; ++i) {
        printf("%d", a[i]);
    }
}
