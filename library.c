#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "library.h"

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
        // d > 0 -> the inequality isn't reversed
        if (d > 0) {
            out.min = min(min_value / d, max_value / d) + 1;
        // d < 0 -> the inequality is reversed. It became '<=' 
        } else if (d < 0) {
            out.max = max(min_value / d, max_value / d);
        }
    } else if (strncmp("<=", op, 2) == 0) {
        if (d > 0) {
            out.max = max(min_value / d, max_value / d);
        } else if (d < 0) {
            out.min = min(min_value / d, max_value / d) + 1;
        }
    } else if (strncmp("<", op, 1) == 0) {
        if (d > 0) {
            out.max = max(min_value / d, max_value / d) - 1;
        } else if (d < 0) {
            out.min = min(min_value / d, max_value / d);
        }
    } else if (strncmp(">=", op, 2) == 0) {
        if (d > 0) {
            out.min = min(min_value / d, max_value / d);
        } else if (d < 0) {
            out.max = max(min_value / d, max_value / d) - 1;
        }
    }

    return out;
}

