struct re;
typedef struct re re_t;

#define MATCH_OFFSET(matches, index)            \
    (matches[2 * index])
#define MATCH_SIZE(matches, index)                      \
    (matches[2 * index + 1] - matches[2 * index])

re_t *re_compile(const char *, int);
int re_match(re_t *, const char *, int, int *);
