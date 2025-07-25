// --- require: ../lib/lib.code ---
// lib.code — vlog standard library externs and basics

// --- Standard includes ---
#include <stddef.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>
#include <stdarg.h>




// --- C Standard Library externs ---
//io
// output


// colors
#define term_RESET "\033[0m"
#define term_BOLD "\033[1m"
#define term_UNDER "\033[4m"

#define term_BLACK "\033[30m"
#define term_RED "\033[31m"
#define term_GREEN "\033[32m"
#define term_YELLOW "\033[33m"
#define term_BLUE "\033[34m"
#define term_MAGENTA "\033[35m"
#define term_CYAN "\033[36m"
#define term_WHITE "\033[37m"

#define term_BGBLACK "\033[40m"
#define term_BGRED "\033[41m"
#define term_BGGREEN "\033[42m"
#define term_BGYELLOW "\033[43m"
#define term_BGRBLUE "\033[44m"
#define term_BGMAG "\033[45m"
#define term_BGCYAN "\033[46m"
#define term_BGWHITE "\033[47m"



// dont use this deperaceted(bad girl\bad boy\bad nd)
int puts(const char *);
//          |
//          |
//          |
// use this V

int printf(const char *restrict, ...);
int fprintf(FILE *restrict stream, const char *restrict format, ...);
int sprintf(char *restrict str, const char *restrict format, ...);
int snprintf(char *restrict str, size_t size, const char *restrict format, ...);
int vprintf(const char *restrict format, va_list ap);
int vfprintf(FILE *restrict stream, const char *restrict format, va_list ap);
int vdprintf(int fd, const char *restrict format, va_list ap);
int vsnprintf(char *restrict str, size_t size, const char *restrict format, va_list ap);

// --- Input ---


// dont use this deperaceted(bad girl\bad boy\bad nd)
extern char *gets(char *s);
//          |
//          |
//          |
// use this V

int scanf(const char *restrict format, ...);
int fscanf(FILE *restrict stream, const char *restrict format, ...);
int sscanf(const char *restrict str, const char *restrict format, ...);
int getchar(void);
int fgetc(FILE *stream);
extern char *fgets(char *s, int size, FILE *stream);

// --- File I/O and Streams ---


// Open and close
extern FILE *fopen(const char *restrict pathname, const char *restrict mode);
extern FILE *freopen(const char *restrict pathname, const char *restrict mode, FILE *restrict stream);
int fclose(FILE *stream);

// Reading
int fgetc(FILE *stream);
extern char *fgets(char *s, int size, FILE *stream);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);

// tmpnam parameter as char pointer (keep it simple, warnings may appear otherwise)

// Writing
int fputc(int c, FILE *stream);
int fputs(const char *s, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

// Formatted input/output
int fprintf(FILE *restrict stream, const char *restrict format, ...);
int fscanf(FILE *restrict stream, const char *restrict format, ...);

// Positioning
int fseek(FILE *stream, long offset, int whence);
long ftell(FILE *stream);
void rewind(FILE *stream);

// Error and EOF
int feof(FILE *stream);
int ferror(FILE *stream);
void clearerr(FILE *stream);

// Temporary files
extern FILE *tmpfile(void);
extern char *tmpnam(char s[L_tmpnam]);

// Buffering
void setbuf(FILE *stream, char *buf);
int setvbuf(FILE *stream, char *buf, int mode, size_t size);

// Flush
int fflush(FILE *stream);

// Standard streams
extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;


// heap
extern void *malloc(size_t);
extern void *calloc(size_t, size_t);
extern void *realloc(void *, size_t);
void free(void *);

void exit(int);
void abort(void);
int atexit(void (*)(void));

// memory operations
extern void *memcpy(void *, const void *, size_t);
extern void *memmove(void *, const void *, size_t);
extern void *memset(void *, int, size_t);
int memcmp(const void *, const void *, size_t);

// string operations
size_t strlen(const char *);
int strcmp(const char *, const char *);
extern char *strcpy(char *, const char *);
extern char *strncpy(char *, const char *, size_t);
extern char *strcat(char *, const char *);
extern char *strncat(char *, const char *, size_t);
extern char *strchr(const char *, int);
extern char *strstr(const char *, const char *);
char* strdup(char* s) {
    const size_t len = strlen(s);;
    char* copy = malloc(len + 1);;
    if (copy) memcpy(copy, s, len + 1);
    return copy;
}


// conversion functions
int atoi(const char *);
long atol(const char *);
double atof(const char *);

// random and time
int rand(void);
void srand(unsigned int);

// --- C time functions ---
long time(long *t);
long clock(void);

// syscall interface
long syscall(long, ...);

// --- vlog standard library functions ---

int rand_int(int min, int max) {
    unsigned int seed = (unsigned int)time(NULL) ^ (unsigned int)clock();;
    seed ^= (unsigned int)min; // Add memory address noise
    srand(seed);
    return (rand() % (max - min + 1)) + min;
}

// --- Simple vector  ---

typedef struct {
    void **items;
    int capacity;
    int length;
} vector;

void vector_init(vector *v) {
    v->capacity = 16;
    v->length = 0;
    v->items = malloc(sizeof(void *) * (size_t)v->capacity);
}

void vector_push(vector *v, void *item) {
    if (v->length >= v->capacity) {
    v->capacity *= 2;
    v->items = realloc(v->items, sizeof(void *) * (size_t)v->capacity);
}
v->items[v->length] = item;
v->length += 1;
}

void* vector_get(vector *v, int index) {
    if (index < 0 || index >= v->length) {
    return NULL;
}
return v->items[index];
}

void vector_free(vector *v) {
    free(v->items);
    v->items = NULL;
    v->capacity = 0;
    v->length = 0;
}

// --- Hashmap implementation ---

typedef struct {
    char *key;
    void *value;
    int in_use; // 0 = empty, 1 = occupied;
} hashmap_entry;

typedef struct {
    hashmap_entry *entries;
    int capacity;
    int length;
} hashmap;

void hashmap_resize(hashmap *map, int new_capacity);
void hashmap_init(hashmap *map, int initial_capacity) {
    map->capacity = initial_capacity > 0 ? initial_capacity : 16;
    map->length = 0;
    map->entries = malloc(sizeof(hashmap_entry) * (size_t)map->capacity);
    for (int i = 0; i < map->capacity; i++) {
    map->entries[i].in_use = 0;
    map->entries[i].key = NULL;
    map->entries[i].value = NULL;
}
}

long djb2_hash(const char *str) {
    unsigned long hash = 5381;;
    int c;
    while ((c = *str++)) {
    hash = ((hash << 5) + hash) + (unsigned char)c; // hash * 33 + c
}
return (long int)hash;
}

void hashmap_set(hashmap *map, char *key, void *value) {
    if (map->length * 2 >= map->capacity) {
    hashmap_resize(map, map->capacity * 2);
}
unsigned long hash = (long unsigned) djb2_hash(key);
int index = (int)(hash % (unsigned long)map->capacity);
while (map->entries[index].in_use == 1) {
if (strcmp(map->entries[index].key, key) == 0) {
map->entries[index].value = value;
return;
}
index = (index + 1) % map->capacity;
}
map->entries[index].key = key;
map->entries[index].value = value;
map->entries[index].in_use = 1;
map->length += 1;
}

void* hashmap_get(hashmap *map, char *key) {
    unsigned long hash = (unsigned)djb2_hash(key);;
    int index = (int)(hash % (unsigned)map->capacity);;
    while (map->entries[index].in_use != 0) {
    if (map->entries[index].in_use == 1 &&
    strcmp(map->entries[index].key, key) == 0) {
    return map->entries[index].value;
}
index = (index + 1) % map->capacity;
}
return NULL;
}

void hashmap_resize(hashmap *map, int new_capacity) {
    hashmap_entry *old_entries = map->entries;
    const int old_capacity = map->capacity;;

    map->entries = malloc(sizeof(hashmap_entry) * (size_t)new_capacity);
    map->capacity = new_capacity;
    map->length = 0;

    for (int i = 0; i < new_capacity; i++) {
    map->entries[i].in_use = 0;
    map->entries[i].key = NULL;
    map->entries[i].value = NULL;
}

for (int i = 0; i < old_capacity; i++) {
if (old_entries[i].in_use == 1) {
hashmap_set(map, old_entries[i].key, old_entries[i].value);
}
}

free(old_entries);
}

void hashmap_free(hashmap *map) {
    free(map->entries);
    map->entries = NULL;
    map->capacity = 0;
    map->length = 0;
}

// String structure and functions

typedef struct {
    char* data;
    size_t len;
    size_t cap;
} String;

String string_new(void) {
    String s;
    s.cap = 16;
    s.len = 0;
    s.data = malloc(s.cap);
    if (s.data != NULL) {
    s.data[0] = '\0';
}
return s;
}

String string_from(char* src) {
    size_t slen = strlen(src);;
    String s;
    s.cap = slen + 1;
    s.len = slen;
    s.data = malloc(s.cap);
    if (s.data != NULL) {
    memcpy(s.data, src, slen + 1);
}
return s;
}

void string_free(String* s) {
    if (s != NULL && s->data != NULL) {
    free(s->data);
    s->data = NULL;
    s->len = 0;
    s->cap = 0;
}
}

void string_push(String* s, char c) {
    if (s->len + 1 >= s->cap) {
    s->cap = s->cap * 2;
    s->data = realloc(s->data, s->cap);
}
s->data[s->len] = c;
s->len = s->len + 1;
s->data[s->len] = '\0';
}

void string_append(String* s, const char* suffix) {
    size_t slen = strlen(suffix);;
    if (s->len + slen >= s->cap) {
    while (s->len + slen >= s->cap) {
    s->cap = s->cap * 2;
}
s->data = realloc(s->data, s->cap);
}
memcpy(s->data + s->len, suffix, slen + 1);
s->len = s->len + slen;
}

void string_clear(String* s) {
    s->len = 0;
    if (s->data != NULL) {
    s->data[0] = '\0';
}
}

// math



#define PI_M 3.1415926535

typedef struct {
    float x;
    float y;
} Vec2;

typedef struct {
    float x;
    float y;
    float z;
} Vec3;



// --- require: ../lib/assert.code ---
#include <assert.h>


typedef struct {
    size_t capacity;
    size_t size;
    int *data;
} Int;

Int* COQ_init_array(size_t size) {
    Int *arr = (Int *)malloc(sizeof(Int));
    if(arr == NULL) assert("Exit null array initalize");
    arr->data = (int *)malloc(size * sizeof(int));
    arr->capacity = size;
    arr->size = 0;
    return arr;
}

void COQ_append_array(Int *array, int item) {
    if (array->capacity <= array->size)
    {
    array->capacity *= 2;
    array->data= (int *)realloc(array->data, array->capacity * sizeof(int));
}
array->data[array->size++] = item;
}

int COQ_get_array_item(Int *array, size_t index) {
    if(index < array->size) return array->data[index];
    else fprintf(stderr, "Index is out of bound");
    return 1;
}

void COQ_free_array(Int *array) {
    free(array->data);
    free(array);
}

int main(void) {

    Int *arr = COQ_init_array(10);
    for (int i = 0; i < 10; ++i)
    {
    COQ_append_array(arr, i);
}
for (int i = 0; i < 10; ++i)
{
printf("%d\n",  COQ_get_array_item(arr, (size_t)i));
}
COQ_free_array(arr);
}
