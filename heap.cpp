#include "general.h"
#include "heap.h"
#include "rtlib.h"
#include "term.h"

#define block_used(b) (1 == ((b)->head.parts.used))
#define block_size(b) ((b)->head.sz & 0x7fffffff)
#define MIN_SPLIT_DELTA 512

// The kernel heap
heap kheap;
// The applications heap
heap appheap;

static const uint32 block_size = sizeof(block);

void heap_init(heap *h, void *start, size_t size) {
  h->start = start;
  h->size = size;
  h->alloc = 0;
  h->f = h->l = h->ff = h->lf = NULL;
}

static void *heap_sbrk(heap *h, int32 size) {

  size_t a = h->alloc;

  if (size < 0) {
    debug_write("Giving back");
  }

  // Maintain word alignment
  size = (size + sizeof(void *) - 1) & ~(sizeof(void *) - 1);

  if (a + size > h->size) {
    return NULL;
  } else if (a + size < 0) {
    panic(L"Incorrect call on heap_sbrk");
  }

  h->alloc = a + size;

  return CAST(void *, CAST(uint8 *, h->start) + a);
}

static inline block *get_block_ptr(void *ptr) {
  // The ptr is the pointer to a data section of a block
  // We can add the size of two void pointer (to be at the end), and substract
  // the block size;
  return CAST(block *, CAST(uint8 *, ptr) + (2 * sizeof(void *)) - block_size);
}

static inline void *get_data_ptr(block *blk) {
  return CAST(void *, &blk->tail.notfree.data);
}

// Find the smallest block of at least the desired size
static block *find_block(heap *h, size_t sz) {
  block *scout = h->ff;

  while (NULL != scout) {
    if (block_size(scout) >= sz) {
      break;
    }
    scout = scout->tail.free.next;
  }

  return scout;
}

static block *split(heap *h, block *bk, size_t min_size) {
  // Fix the min size so we can break on multiple of four
  if (min_size % 4 != 0) {
    min_size += 4 - (min_size % 4);
  }

  uint8 *new_end_of_zone =
      CAST(uint8 *, bk) + (min_size + block_size - (2 * sizeof(void *)));

  block *left_bound = bk->prev;
  block *right_bound = bk->next;

  block *other_half = CAST(block *, new_end_of_zone);

  other_half->head.sz =
      (bk->head.sz - min_size) - block_size + (2 * sizeof(void *));

  bk->head.sz = min_size;

  other_half->head.parts.used = 0;

  bk->prev = left_bound;
  bk->next = other_half;
  other_half->prev = bk;
  other_half->next = right_bound;

  if (NULL != right_bound) {
    right_bound->prev = other_half;
  } else {
    h->l = other_half;
  }

  if ((NULL != right_bound) && CAST(uint8 *, right_bound) !=
                                   (CAST(uint8 *, other_half) + block_size +
                                    other_half->head.sz - 2 * sizeof(void *))) {
    debug_write(CAST(uint8 *, right_bound));
    debug_write((CAST(uint8 *, other_half) + block_size + other_half->head.sz -
                 2 * sizeof(void *)));
    panic(L"!");
  }

  return other_half;
}

/**
 * Extend the heap enough to have a block of size sz.
 */
static block *extend_heap(heap *h, size_t sz) {
  // There is no cost in extending exactly the size we need.
  size_t total_sz = sz + block_size - (sizeof(void *) * 2);

  block *blk = CAST(block *, heap_sbrk(h, total_sz));

  if (NULL == blk) {
    return NULL;
  }

  blk->head.parts.used = 0;
  uint32 diff =
      ((CAST(uint8 *, h->start) + h->alloc) - CAST(uint8 *, blk)) - total_sz;
  blk->head.sz = sz + diff;

  blk->next = NULL;
  blk->prev = h->l;

  if (NULL != h->l) {
    h->l->next = blk;
  }

  if (NULL == h->f) {
    h->f = blk;
  }

  h->l = blk;


  return blk;
}

void heap_free(heap *h, void *ptr) {

static void detach(heap *h, block *bl) {

  block *prev = bl->tail.free.prev;
  block *next = bl->tail.free.next;

  if (NULL != prev) {
    prev->tail.free.next = next;
  } else {
    // bl must have been the first
    h->ff = next;
  }

  if (NULL != next) {
    next->tail.free.prev = prev;
  } else {
    // bl must have been the last
    h->lf = prev;
  }
}


static void mark_as_free(heap *h, block *bk) {
  bk->tail.free.next = NULL;
  bk->tail.free.prev = NULL;
  bk->head.parts.used = 0;

  block *scout = h->lf;

  while (NULL != scout && (block_size(scout) >= block_size(bk))) {
    scout = scout->tail.free.prev;
  }

  if (NULL == scout) { // we are the first one
    block *old_first = h->ff;
    old_first->tail.free.prev = bk;
    bk->tail.free.next = old_first;
    h->ff = bk;
  } else {
    // We insert after scout
    block *old_next = scout->tail.free.next;

    if (NULL == old_next) { // end of chain
      h->lf = bk;
    } else {
      old_next->tail.free.prev = bk;
      bk->tail.free.next = old_next;
    }
    scout->tail.free.next = bk;
    bk->tail.free.prev = scout;
  }
}

static block *get_block(heap *h, size_t sz) {
  block *blk = find_block(h, sz);
  if (NULL == blk) {
    // need to allocate
    blk = extend_heap(h, sz);
  } else {
    detach(h, blk);
    if (block_size(blk) >= MIN_SPLIT_DELTA + sz) {
      block *other_half = split(h, blk, sz);
      mark_as_free(h, other_half);
    }
  }

  if (NULL != blk) {
    blk->head.parts.used = 1;
  }

  return blk;
}

static block *merge(heap *h, block *leftmost, uint8 until) {
  block *scout = leftmost;
  block *left_bound = leftmost->prev;
  block *right_bound = NULL;
  size_t nsize = 0;
  for (uint8 i = 0; i < until; ++i) {
    if (!block_used(scout)) {
      detach(h, scout);
    }
    nsize += block_size + scout->head.sz - (2 * sizeof(block *));
    scout = scout->next;
  }

  right_bound = scout;

  leftmost->prev = left_bound;
  leftmost->next = right_bound;

  if (NULL != left_bound) {
    left_bound->next = leftmost;
  } else {
    // leftmost is the start of the chain
    h->f = leftmost;
  }

  if (NULL != right_bound) {
    right_bound->prev = leftmost;
  } else {
    // The last block we merged was the last one
    h->l = leftmost;
  }

  leftmost->head.sz = nsize - block_size + (2 * sizeof(void *));
  leftmost->head.parts.used = 0;

  return leftmost;
}

void heap_free(heap *h, void *ptr) {
  block *bk = get_block_ptr(ptr);

  block *left = bk->prev;
  block *right = bk->next;

  bool left_free = (NULL != left && !block_used(left));
  bool right_free = (NULL != right && !block_used(right));

  if (left_free && right_free) {
    bk = merge(h, left, 2);
  } else if (left_free) {
    bk = merge(h, left, 1);
  } else if (right_free) {
    bk = merge(h, bk, 1);
  }

  mark_as_free(h, bk);
}

#define MIN_ALLOC_SIZE (2 * sizeof(void *))

void *heap_malloc(heap *h, size_t size) {

  if (size < MIN_ALLOC_SIZE) {
    size = MIN_ALLOC_SIZE;
  }

  block *blk = get_block(h, size);

  if (NULL == blk) {
    return NULL;
  }

  void *ptr = get_data_ptr(blk);

  if ((CAST(uint32, ptr) % 4) != 0) {
    panic(L"Not aligned");
  }

  return ptr;
}

#undef block_used
#undef block_size
#undef MIN_SPLIT_DELTA
