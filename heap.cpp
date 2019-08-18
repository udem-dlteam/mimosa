#include "heap.h"
#include "general.h"
#include "rtlib.h"
#include "term.h"

// The kernel heap
heap kheap;
heap appheap;

static const uint32 mem_block_size = sizeof(mem_block);

static int byte_alignement = 4;

void set_alignement(int alignement) {
    byte_alignement = alignement;
}

size_t align(size_t to_align) {
    return to_align + (to_align % byte_alignement);
}

void heap_init(heap *h, void *start, size_t size) {
  h->start = start;
  h->size = size;
  h->alloc = 0;
}

static void *heap_sbrk(heap *h, int32 size) {

  size_t a = h->alloc;

  if(size < 0) {
      debug_write("Giving back");
  }
    
  if (a + size > h->size) {
    debug_write("HEAP OVERFLOW");
    return NULL; // heap overflow
  } else if (a + size < 0) {
    panic(L"Incorrect call on heap_sbrk");
  }

  h->alloc = a + size;

  return CAST(void*,CAST(char*,h->start)+a);
}

/*
 * Convenience methods to get the data address of a block and the block from a data address
 * We use char pointer since they are each 8 bit, therefore we are certain
 * that we are making the maths on bytes and not a byte multiple.
 */

static inline mem_block *get_block_header(void *address) {
    char *tmp = CAST(char*, address);
    tmp -= mem_block_size;
    return (mem_block *) tmp;
}

static inline void *get_data_address(mem_block *header) {
    char *p = (char *) header;
    p += mem_block_size;
    return p;
}

/*
 * Actual memory operations
 */

/**
 * Give back memory to the system
 * It allows actual memory management
 * and not only take only
 */
static void give_back_to_system(heap* h) {

    mem_block *block_to_give_back = h->latest_block_in_chain;

    if (NULL != block_to_give_back->last) {
        block_to_give_back->last->next = NULL;
        h->latest_block_in_chain = block_to_give_back->last;
    } else {
        h->latest_block_in_chain = NULL;
        h->first_block_in_chain = NULL;
    }

    if (NULL != block_to_give_back->last_free) {
        block_to_give_back->last_free->next_free = NULL;
    }

    if (block_to_give_back == h->first_free) {
        h->first_free = NULL;
    }

    size_t size_to_give_back = (block_to_give_back->size + mem_block_size);
    heap_sbrk(h, -size_to_give_back);
}

/**
 * Merge a block with it's following block
 * It allows to sum up the sizes and reuse
 * size without extending the heap
 * @param block
 */
static void merge(heap* h, mem_block *block) {
    mem_block *next = block->next;

    block->size += (mem_block_size + next->size);

    //remove the block link
    mem_block *new_next = next->next;

    if (NULL != new_next) {
        new_next->last = block;
        block->next = new_next;
    } else {
        h->latest_block_in_chain = block;
        block->next = NULL;
    }
}

/**
 * Frees a block of memory.
 * It will try to merge free blocks to allow larger mallocs later
 * If the last block of the chain is free, it will release resources
 * to the system
 * @param pointer (ptr) of the block
 */
void heap_free(heap* h, void *pointer) {
    mem_block *block = get_block_header(pointer);

#ifdef DEBUG
    printf("Releasing block at %p\r\n", block);
    ++free_blocks;
#endif

    block->used = 0;

    mem_block *next = NULL;

    if ((NULL != block->next) && (!block->next->used)) {
        merge(h, block);
        next = block->next;
    }

    if ((NULL != block->last) && (!block->last->used)) {
        merge(h, block->last);
        next = block;
        block = next->last;
    }

    //relinks
    if (NULL != next && NULL != next->last_free) {
        next->last_free->next_free = block;
        block->last_free = next->last_free;
    } else {
        block->last_free = NULL;
    }

    if (NULL != next && NULL != next->next_free) {
        next->next_free->last_free = block;
        block->next_free = next->next_free;
    } else {
        block->next_free = NULL;
    }

    if (NULL != h->first_free && (block != h->first_free)) {
        block->next_free = h->first_free;
        h->first_free->last_free = block;
        h->first_free = block;
        h->first_free->last_free = NULL;
    } else {
        h->first_free = block;
    }

    if (!h->latest_block_in_chain->used) {
        give_back_to_system(h);
    }
}

/**
 * Find the first block that has the required size and that is
 * available to be used
 *
 * @param size_required the size required by the user
 * @return a pointer on the block that is available
 */
static mem_block *find_block(heap *h, size_t size_required) {
  mem_block *block = h->first_free;
  // TODO use the actual free list
  while (NULL != block) {
    if ((!block->used) && (block->size >= size_required)) {
      return block;
    } else {
      block = block->next_free;
    }
  }

  return NULL;
}

static void *extend_heap(heap *h, size_t size) {
  void *current = heap_sbrk(h, 0);

  size_t to_alloc = mem_block_size + size;
  heap_sbrk(h, to_alloc);

  return current;
}

/**
 * Split a block so we can use a part of it and leave the other part to
 * another allocation
 * @param block
 * @param required_size
 */
static void split(heap* h, mem_block *block, size_t required_size) {
    mem_block *after_block = block->next;

    size_t original_size = block->size;

    char *data = CAST(char*, get_data_address(block));
    char *end = data + required_size;

    mem_block *new_block = (mem_block *) end;

    //relink
    block->next = new_block;

    block->size = required_size;
    new_block->size = original_size - (required_size + mem_block_size);
    new_block->used = 0;
    new_block->last = block;
    new_block->next_free = NULL;
    new_block->last_free = NULL;

    if (NULL != after_block) {
        new_block->next = after_block;
        after_block->last = new_block;
    } else {
        h->latest_block_in_chain = new_block;
        new_block->next = NULL;
    }

    if (NULL != block->next_free) {
        new_block->next_free = block->next_free;
        block->next_free->last_free = new_block;
        block->next_free = new_block; //will probably never be used but just in case we protect
    }

    //since we split with the second half
    if (h->first_free == block) {
        h->first_free = new_block;
    }
}

static mem_block* get_block(heap* h, size_t required_size) {

    required_size = align(required_size);

    mem_block *found = NULL;

    if (NULL == h->first_block_in_chain) {
        //first allocation ever
        found = CAST(mem_block*, extend_heap(h, required_size));
        h->first_block_in_chain = found;
        found->size = required_size;
        h->latest_block_in_chain = found;
        h->latest_block_in_chain->next = NULL;
        h->latest_block_in_chain->last = NULL;
    } else if (NULL == (found = find_block(h, required_size))) {
        //we did not find any suitable block
        found = CAST(mem_block*, extend_heap(h, required_size));
        mem_block *previous_last = h->latest_block_in_chain;
        //link it back
        previous_last->next = found;

        found->size = required_size;
        h->latest_block_in_chain = found;

        found->last = previous_last;
        h->latest_block_in_chain->next = NULL;
    } else {
        //we found a suitable block we can reuse
        //we need to split the block if we got a huge one
        if (found->size > (required_size + mem_block_size)) {
            split(h, found, required_size);
        }
    }

    found->used = 1;

    //Relink the free block chain
    if (NULL != found->last_free) {
        found->last_free->next_free = found->next_free;
    }

    if (NULL != found->next_free) {
        found->next_free->last_free = found->last_free;
    }

    return found;
}

/**
 * Allocate a block of memory.
 * It will extend the heap if it cannot find a suitable
 * block already freed
 * @param required_size the size required. It will be aligned according to the desired
 * alignement
 * @return a pointer that points towards the data block
 */
void *heap_malloc(heap* h, size_t required_size) {
    mem_block *found = get_block(h, required_size);
    return get_data_address(found);
}
