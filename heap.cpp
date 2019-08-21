#include "heap.h"
#include "general.h"
#include "rtlib.h"
#include "term.h"

// The kernel heap
heap kheap;
// The applications heap
heap appheap;

static const uint32 mem_block_size = sizeof(mem_block);

void heap_init(heap *h, void *start, size_t size) {
  h->start = start;
  h->size = size;
  h->alloc = 0;
  h->last_block_in_chain = NULL;
  h->first_block_in_chain = NULL;
  h->first_free = NULL;
}

static void *heap_sbrk(heap *h, int32 size) {

  size_t a = h->alloc;

  if(size < 0) {
      debug_write("Giving back");
  }

  // Maintain word alignment
  size = (size + sizeof(void*) - 1) & ~(sizeof(void*) - 1);
    
  if (a + size > h->size) {
    panic(L"Heap overflow");
  } else if (a + size < 0) {
    panic(L"Incorrect call on heap_sbrk");
  }

  h->alloc = a + size;

  return CAST(void*,CAST(uint8*,h->start)+a);
}

static inline mem_block* get_block_ptr(void* ptr) {
    return CAST(mem_block*, CAST(char*, ptr) - mem_block_size);
}

static inline void* get_data_ptr(mem_block* bk) {
    return CAST(void*, CAST(char*, bk) + mem_block_size);
}

static mem_block* get_block(heap* h, size_t sz) {
  mem_block* bk = h->first_free;

  while (NULL != bk && bk->sz < sz) {
    bk = bk->next_free;
  }

  if (NULL == bk) {
    // It is usually expensive to call sbrk. Since we are only
    // doing pointers arithmetics, it is cheap and we can afford
    // extending by the block size we want
    bk = CAST(mem_block*, heap_sbrk(h, sz + mem_block_size));

    if (NULL == bk) return bk;

    if (NULL == h->first_block_in_chain) {
      h->first_block_in_chain = bk;
    }

    mem_block* last = h->last_block_in_chain;

    if (NULL != last) {
      last->next = bk;
      bk->prev = last;
    }

    h->last_block_in_chain = bk;
  } else {
    mem_block* prev_free = bk->prev_free;
    mem_block* next_free = bk->next_free;

    if (NULL != prev_free) {
      prev_free->next_free = next_free;
    }

    if (NULL != next_free) {
      next_free->prev_free = prev_free;
    }
  }

  bk->next_free = bk->prev_free = NULL;
  bk->used = 1;

#ifdef KIND_MALLOC

  uint8* data_ptr = CAST(uint8*, get_data_ptr(bk));

  for (uint32 i = 0; i < sz; ++i) {
    data_ptr[i] = 0;
  }

#endif

  return bk;
}

void heap_free(heap* h, void* ptr) {
    mem_block* bk = get_block_ptr(ptr);

    if(!bk->used) {
      panic(L"Freeing a non-allocated block");
    }

    bk->used = 0;

    mem_block* old_first = h->first_free;
    h->first_free = bk;

    if (NULL != old_first) {
      bk->next_free = old_first;
      old_first->prev_free = bk;
    }
}

void* heap_malloc(heap* h, size_t size) {
  void* bk = get_data_ptr(get_block(h, size));

  if ((CAST(uint32, bk) % 4) != 0) {
    panic(L"Not aligned");
  }

  return bk;
}
