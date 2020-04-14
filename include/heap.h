#ifndef __MEMORY_H
#define __MEMORY_H

#include "general.h"

#define END_KERNEL_HEAP (64 * (1 << 20))

typedef struct heap_struct heap;
typedef struct mem_block_struct block;
typedef struct free_block_struct free_block;

struct mem_block_struct {
  // The length is stored as an uint32
  // the top bit indicate if the block is in use
  // the rest of the int is reserved for the size
  union {
    struct {
      uint8 used : 1;
      uint8 top : 7;
      uint8 data1;
      uint8 data2;
      uint8 data3;
    } parts;
    // The size of the block is only the size of the
    // data section. The total size is sizeof(block) + block.head.sz - 2 *
    // sizeof(block*) The unit of sz is in chars.
    uint32 sz;
  } head;
  block *next;
  block *prev;
  // Those fields are valid iff the
  // block is not freeed
  // Otherwise, they are given to the block data section: &next_free is the
  // start of the data section
  union {
    struct {
      block *next;
      block *prev;
    } free;
    struct {
      void *data;
      void *padding;
    } notfree;
  } tail;
};

struct heap_struct {
  void *start;
  // size of the heap
  size_t size;
  // allocated size
  size_t alloc;
  // first
  block *f;
  // last
  block *l;
  // first free (smallest to biggest size)
  block *ff;
  // last free (smallest to biggest size)
  block *lf;
};

void heap_init(heap *h, void *start, size_t size);

void *heap_malloc(heap *h, size_t size);

void heap_free(heap *h, void *ptr);

extern heap kheap;
extern heap appheap;

#endif
