int foo;
int *foo;
tname foo;
static tname foo;
tname static foo;

const int *foo;
int * const foo;
int ***const* foo;

int foo(int);
int (foo)(int);
int (*foo)(int);
int (*(*foo(int)))(int);
int (*(*foo)(int))(int);
int foo(int)(int);

int __attribute__((x)) foo;
int foo(int) __attribute__((x));


int blah foo(int);

inline int foo(int);
