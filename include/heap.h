#ifndef __HEAP_H
#define __HEAP_H

struct heap {
  void *start;
  size_t size;
  size_t alloc;
};

void heap_init(struct heap *h, void *start, size_t size);
void *heap_malloc(struct heap *h, size_t size);
void heap_free(struct heap *h, void *ptr);

#endif