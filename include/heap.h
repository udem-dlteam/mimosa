#ifndef __MEMORY_H
#define __MEMORY_H

#include "general.h"

typedef struct heap_struct heap;
typedef struct mem_block_struct mem_block;

struct mem_block_struct {
    int32 size;
    char used;
    mem_block *next;
    mem_block *last;
    mem_block *next_free;
    mem_block *last_free;
};

struct heap_struct {
  void *start;
  size_t size;
  size_t alloc;
  mem_block *latest_block_in_chain;
  mem_block *first_block_in_chain;
  mem_block *first_free;
};

void heap_init(heap *h, void *start, size_t size);

void *heap_malloc(heap *h, size_t size);

void heap_free(heap *h, void *ptr);

extern heap kheap;
extern heap appheap;

#endif