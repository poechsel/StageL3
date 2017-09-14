#ifndef LIBRARY_H
#define LIBRARY_H 


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

int min(int a, int b);
int max(int a, int b);

int min3(int a, int b, int c);
int max3(int a, int b, int c);

struct s_iterator INTER_INTERVAL(struct s_iterator a, struct s_iterator b);
struct s_iterator UNION_INTERVAL(struct s_iterator a, struct s_iterator b);

struct s_iterator MAKE_OP_INTERVAL (char* op, int min_value, int max_value, int minus_div);


#endif /* LIBRARY_H */
