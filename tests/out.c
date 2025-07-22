// --- require: ../lib/lib.code ---
// lib.code — vlog standard library externs and basics

// --- Standard includes ---
#include <stddef.h>
#include <sys/syscall.h>
#include <unistd.h>
#include "raylib.h"


// --- C Standard Library externs ---
int puts(const char *);
int printf(const char *restrict, ...);

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
long time(void *);

// syscall interface
long syscall(long, ...);

// --- vlog standard library functions ---

int rand_int(int min, int max) {
    srand((unsigned int)time(NULL));
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

#include <math.h>


#define PI 3.1415926535

typedef struct {
    float x;
    float y;
} Vec2;

typedef struct {
    float x;
    float y;
    float z;
} Vec3;



int main(void) {
    hashmap map;
    hashmap_init(&map, 16);

    char* key1 = "foo";;
    char* key2 = "bar";;

    int value1 = 42;;
    int value2 = 1337;;

    hashmap_set(&map, key1, &value1);
    hashmap_set(&map, key2, &value2);

    int *v1 = (int *)hashmap_get(&map, key1);
    int *v2 = (int *)hashmap_get(&map, key2);

    if (v1 != NULL) {
    printf("Value for %s is %d\n", key1, *v1);
}
if (v2 != NULL) {
printf("Value for %s is %d\n", key2, *v2);
}

int r = rand_int(1, 100);
printf("Random number between 1 and 100: %d\n", r);

hashmap_free(&map);

return 0;
}
