#ifndef __MEMORY_H
#define __MEMORY_H

#include "general.h"

typedef struct heap_struct heap;
typedef struct mem_block_struct mem_block;

struct mem_block_struct {
    uint8 used;
    uint32 sz;
    mem_block *next;
    mem_block *prev;
    mem_block *next_free;
    mem_block *prev_free;
};

struct heap_struct {
  void *start;
  size_t size;
  size_t alloc;
  mem_block *last_block_in_chain;
  mem_block *first_block_in_chain;
  mem_block *first_free;
};

void heap_init(heap *h, void *start, size_t size);

void *heap_malloc(heap *h, size_t size);

void heap_free(heap *h, void *ptr);

extern heap kheap;
extern heap appheap;

#endif
