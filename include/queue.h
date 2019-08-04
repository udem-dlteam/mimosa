/* file: "queue.h" */

/*
 * Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
 * Rights Reserved.
 *
 * Revision History
 * 02 Nov 01  initial version (Marc Feeley)
 */

/*
 * This file is normally included with "#include" to generate a set of
 * queue routines.  It should be included once for every set of queue
 * routines that is needed.  In C++ we would normally use templates to
 * achieve this goal, but in C the "#include" approach is required.
 * Just before including the file the following macros must be
 * defined:
 *
 *   NODETYPE                the base type for the queue and its elements
 *   QUEUETYPE               the NODETYPE subtype for the queue
 *   ELEMTYPE                the NODETYPE subtype for the elements of the queue
 *   NAMESPACE_PREFIX(name)  creates a prefixed name to avoid name clashes
 *
 *   if USE_DOUBLY_LINKED_LIST is defined:
 *     BEFORE(elem1,elem2)
 *     NEXT(node)
 *     NEXT_SET(node,next_node)
 *     PREV(node)
 *     PREV_SET(node,prev_node)
 *
 *   if USE_RED_BLACK_TREE is defined:
 *     BEFORE(elem1,elem2)
 *     COLOR(node)
 *     COLOR_SET(node,queue)
 *     PARENT(node)
 *     PARENT_SET(node,parent_node)
 *     LEFT(node)
 *     LEFT_SET(node,left_node)
 *     RIGHT(node)
 *     RIGHT_SET(node,right_node)
 *     LEFTMOST(queue)
 *     LEFTMOST_SET(queue,node)
 *     RIGHTMOST(queue)
 *     RIGHTMOST_SET(queue,node)
 */

/*---------------------------------------------------------------------------*/

#ifdef USE_DOUBLY_LINKED_LIST

/*
 * Priority queue implementation using doubly-linked lists.
 */


inline void NAMESPACE_PREFIX(init) (QUEUETYPE* queue)
{
  NEXT_SET (CAST(NODETYPE*,queue), CAST(NODETYPE*,queue));
  PREV_SET (CAST(NODETYPE*,queue), CAST(NODETYPE*,queue));
}


inline void NAMESPACE_PREFIX(detach) (ELEMTYPE* elem)
{
  NEXT_SET (CAST(NODETYPE*,elem), CAST(NODETYPE*,elem));
  PREV_SET (CAST(NODETYPE*,elem), CAST(NODETYPE*,elem));
}


inline ELEMTYPE* NAMESPACE_PREFIX(head) (QUEUETYPE* queue)
{
  NODETYPE* head = NEXT (CAST(NODETYPE*,queue));

  if (head != CAST(NODETYPE*,queue))
    return CAST(ELEMTYPE*,head);

  return NULL;
}

extern condvar* circular_buffer_cv;

inline void NAMESPACE_PREFIX(insert) (ELEMTYPE* elem, QUEUETYPE* queue)
{
  NODETYPE* node2 = CAST(NODETYPE*,queue);
  NODETYPE* node1 = PREV (node2);

  while (node1 != CAST(NODETYPE*, queue) &&
         BEFORE(elem, CAST(ELEMTYPE*, node1))) {
    node2 = node1;
    node1 = PREV(node2);
  }

  // insert elem between node1 and node2

  NEXT_SET (node1, CAST(NODETYPE*,elem));
  PREV_SET (CAST(NODETYPE*,elem), node1);

  NEXT_SET (CAST(NODETYPE*,elem), node2);
  PREV_SET (node2, CAST(NODETYPE*,elem));
}


inline void NAMESPACE_PREFIX(remove) (ELEMTYPE* elem)
{
  NODETYPE* prev_node = PREV (CAST(NODETYPE*,elem));
  NODETYPE* next_node = NEXT (CAST(NODETYPE*,elem));

  NEXT_SET (prev_node, next_node);
  PREV_SET (next_node, prev_node);
}

#endif


/*---------------------------------------------------------------------------*/

#ifdef USE_RED_BLACK_TREE

/*
 * Priority queue implementation using red-black trees.
 */

/*
 * The red-black tree implementation used here is inspired from the
 * code in the MIT-Scheme runtime (file "rbtree.scm").  That code is
 * based on the algorithms presented in the book "Introduction to
 * Algorithms" by Cormen, Leiserson, and Rivest.
 *
 * The main differences with the MIT-Scheme code are:
 *
 *   1) Nil pointers are replaced by a special sentinel that is also
 *      the cell that contains a pointer (in the "left child" field)
 *      to the root of the red-black tree.  The "right child" field of
 *      the sentinel is never accessed.  The sentinel is black.
 *
 *   2) The color field contains NULL when the node is red and a
 *      reference to the sentinel when the node is black.  It is thus
 *      possible to find the sentinel from any node in constant time
 *      (if the node is black extract the color field, otherwise
 *      extract the color field of the parent, which must be black).
 *
 *   3) One field of the sentinel always points to the leftmost node of
 *      the red-black tree.  This allows constant time access to the
 *      "minimum" node, which is a frequent operation of priority queues.
 *
 *   4) Several cases are handled specially (see the code for details).
 *
 *   5) Macros are used to generate code specialized for each case of
 *      symmetrical operations (e.g. left and right rotation).
 *
 *   6) Nodes are assumed to be preallocated.  Inserting and deleting a
 *      node from a red-black tree does not perform any heap
 *      allocation.  Moreover, all algorithms consume a constant amount
 *      of stack space.
 */

#define MAINTAIN_LEFTMOST

#define BLACKEN(node,queue) COLOR_SET (node, queue)
#define IS_BLACK(node) COLOR (node) != NULL

#define REDEN(node) COLOR_SET (node, NULL)
#define IS_RED(node) COLOR (node) == NULL

#define COPY_COLOR(dst_node,src_node) \
COLOR_SET (dst_node, COLOR (src_node))

#define EXCHANGE_COLOR(node1,node2) \
do { \
     QUEUETYPE* tempcolor = COLOR (node1); \
     COLOR_SET (node1, COLOR (node2)); \
     COLOR_SET (node2, tempcolor); \
   } while (0)

#define UPDATE_PARENT(parent_node,old_node,new_node) \
do { \
     if (old_node == LEFT (parent_node)) \
       LEFT_SET (parent_node, new_node); \
     else \
       RIGHT_SET (parent_node, new_node); \
   } while (0)

#define ROTATE(node,side1,side1_set,side2,side2_set) \
do { \
     NODETYPE* x = node; \
     NODETYPE* side2_x; \
     NODETYPE* side1_side2_x; \
     NODETYPE* parent_x; \
     side2_x = side2 (x); \
     side1_side2_x = side1 (side2_x); \
     side2_set (x, side1_side2_x); \
     PARENT_SET (side1_side2_x, x); \
     parent_x = PARENT (x); \
     side1_set (side2_x, x); \
     PARENT_SET (x, side2_x); \
     PARENT_SET (side2_x, parent_x); \
     UPDATE_PARENT (parent_x, x, side2_x); \
   } while (0)

#define ROTATE_LEFT(node) ROTATE(node,LEFT,LEFT_SET,RIGHT,RIGHT_SET)
#define ROTATE_RIGHT(node) ROTATE(node,RIGHT,RIGHT_SET,LEFT,LEFT_SET)

#define GET_QUEUE(node,var) \
do { \
     var = COLOR (node); \
     if (var == NULL) \
       var = COLOR (PARENT (node)); \
   } while (0)


inline void NAMESPACE_PREFIX(init) (QUEUETYPE* queue)
{
  //  cout << "[init]";////////////////
  PARENT_SET (CAST(NODETYPE*,queue), CAST(NODETYPE*,queue));
  LEFT_SET (CAST(NODETYPE*,queue), CAST(NODETYPE*,queue));
  BLACKEN (CAST(NODETYPE*,queue), queue);

#ifdef MAINTAIN_LEFTMOST
  LEFTMOST_SET (queue, CAST(NODETYPE*,queue));
#endif

#ifdef MAINTAIN_RIGHTMOST
  RIGHTMOST_SET (queue, CAST(NODETYPE*,queue));
#endif
}


inline ELEMTYPE* NAMESPACE_PREFIX(head) (QUEUETYPE* queue)
{
  //  cout << "[head]";////////////////
  NODETYPE* head = LEFTMOST (queue);

  if (head != CAST(NODETYPE*,queue))
    return CAST(ELEMTYPE*,head);

  return NULL;
}


inline void NAMESPACE_PREFIX(insert) (ELEMTYPE* elem, QUEUETYPE* queue)
{
  //  cout << "[insert]";////////////////
  NODETYPE* node = CAST(NODETYPE*,elem);
  NODETYPE* x;
  NODETYPE* side_x;

  REDEN (node);
  LEFT_SET (node, CAST(NODETYPE*,queue));
  RIGHT_SET (node, CAST(NODETYPE*,queue));

  x = CAST(NODETYPE*,queue);

  goto insert_left;

  for (;;)
    {
      x = side_x;
      if (BEFORE (CAST(ELEMTYPE*,node), CAST(ELEMTYPE*,x)))
        {
        insert_left:

          side_x = LEFT (x);

          if (side_x == CAST(NODETYPE*,queue))
            {
              LEFT_SET (x, node);
              PARENT_SET (node, x);
#ifdef MAINTAIN_LEFTMOST
              if (x == LEFTMOST (queue))
                LEFTMOST_SET (queue, node);
#endif
              break;
            }
        }
      else
        {
          side_x = RIGHT (x);

          if (side_x == CAST(NODETYPE*,queue))
            {
              RIGHT_SET (x, node);
              PARENT_SET (node, x);
#ifdef MAINTAIN_RIGHTMOST
              if (x == RIGHTMOST (queue))
                RIGHTMOST_SET (queue, node);
#endif
              break;
            }
        }
    }

  /* fixup the tree so that it remains properly balanced */

  x = node;

  for (;;)
    {
      NODETYPE* parent_x = PARENT (x);
      NODETYPE* parent_parent_x;

      if (IS_BLACK (parent_x))
        break;

      parent_parent_x = PARENT (parent_x);

#define INSERT_BODY(side1,rotate_side1,side2,rotate_side2) \
{ /* "do ... while (0)" can't be used because the body contains "break" */ \
  NODETYPE* side2_parent_parent_x = side2 (parent_parent_x); \
  if (IS_BLACK (side2_parent_parent_x)) \
    { \
      NODETYPE* y; \
      NODETYPE* parent_y; \
      if (x == side2 (parent_x)) \
        { \
          rotate_side1 (parent_x); \
          y = PARENT (parent_x); \
        } \
      else \
        y = PARENT (x); \
      BLACKEN (y, queue); \
      parent_y = PARENT (y); \
      REDEN (parent_y); \
      rotate_side2 (parent_y); \
      break; \
    } \
  BLACKEN (parent_x, queue); \
  BLACKEN (side2_parent_parent_x, queue); \
  REDEN (parent_parent_x); \
  x = parent_parent_x; \
}

      if (parent_x == LEFT (parent_parent_x))
        INSERT_BODY (LEFT, ROTATE_LEFT, RIGHT, ROTATE_RIGHT) /* no ; needed */
      else
        INSERT_BODY (RIGHT, ROTATE_RIGHT, LEFT, ROTATE_LEFT) /* no ; needed */
    }

  BLACKEN (LEFT (CAST(NODETYPE*,queue)), queue);
  PARENT_SET (CAST(NODETYPE*,queue), CAST(NODETYPE*,queue));
}


inline void NAMESPACE_PREFIX(remove) (ELEMTYPE* elem)
{
  //  cout << "[remove]";////////////////
  NODETYPE* node = CAST(NODETYPE*,elem);
  QUEUETYPE* queue;
  NODETYPE* parent_node;
  NODETYPE* left_node;
  NODETYPE* right_node;

  GET_QUEUE (node, queue);

  parent_node = PARENT (node);
  left_node = LEFT (node);
  right_node = RIGHT (node);

  PARENT_SET (node, NULL); /* to avoid dangling references */
  LEFT_SET (node, NULL);
  RIGHT_SET (node, NULL);

  if (left_node == CAST(NODETYPE*,queue))
    {
#ifdef MAINTAIN_LEFTMOST

      /* check if leftmost must be updated */

      if (node == LEFTMOST (queue))
        {
          if (right_node == CAST(NODETYPE*,queue))
            LEFTMOST_SET (queue, parent_node);
          else
            LEFTMOST_SET (queue, right_node);
        }

#endif

      PARENT_SET (right_node, parent_node);
      UPDATE_PARENT (parent_node, node, right_node);

      if (IS_RED (node))
        goto rbtree_is_balanced;

      REDEN (node); /* to avoid dangling references */
      node = right_node;
    }
  else if (right_node == CAST(NODETYPE*,queue))
    {
#ifdef MAINTAIN_RIGHTMOST

      /* check if rightmost must be updated */

      if (node == RIGHTMOST (queue))
        RIGHTMOST_SET (queue, left_node);

#endif

      PARENT_SET (left_node, parent_node);
      UPDATE_PARENT (parent_node, node, left_node);

      /* 
       * At this point we know that the node is black.  This is
       * because the right child is nil and the left child is red (if
       * the left child was black the tree would not be balanced).
       */

      REDEN (node); /* to avoid dangling references */
      node = left_node;
    }
  else
    {
      NODETYPE* x = right_node;
      NODETYPE* parent_x = node;

      for (;;)
        {
          NODETYPE* left_x = LEFT (x);
          //  cout << "[1]";////////////////

          if (left_x == CAST(NODETYPE*,queue))
            break;

          parent_x = x;
          x = left_x;
        }

      EXCHANGE_COLOR (x, node);
      PARENT_SET (left_node, x);
      LEFT_SET (x, left_node);
      PARENT_SET (x, parent_node);
      UPDATE_PARENT (parent_node, node, x);

      if (x == right_node)
        {
          if (IS_RED (node))
            goto rbtree_is_balanced;

          REDEN (node); /* to avoid dangling references */
          parent_node = x;
          node = RIGHT (x);
        }
      else
        {
          NODETYPE* right_x = RIGHT (x);

          PARENT_SET (right_x, parent_x);
          LEFT_SET (parent_x, right_x);
          PARENT_SET (right_node, x);
          RIGHT_SET (x, right_node);

          if (IS_RED (node))
            goto rbtree_is_balanced;

          REDEN (node); /* to avoid dangling references */
          parent_node = parent_x;
          node = right_x;
        }
    }

  /* fixup the tree so that it remains properly balanced */

#define REMOVE_BODY(side1,rotate_side1,side2,rotate_side2) \
{ /* "do ... while (0)" can't be used because the body contains "break" */ \
  NODETYPE* x; \
  NODETYPE* side1_x; \
  NODETYPE* side2_parent_node = side2 (parent_node); \
  if (IS_RED (side2_parent_node)) \
    { \
      BLACKEN (side2_parent_node, queue); \
      REDEN (parent_node); \
      rotate_side1 (parent_node); \
      x = side2 (parent_node); \
    } \
  else \
    x = side2_parent_node; \
  if (IS_RED (side2 (x))) \
    { \
      COMMON_CASE (x, side2, rotate_side1); \
      break; \
    } \
  side1_x = side1 (x); \
  if (IS_RED (side1_x)) \
    { \
      BLACKEN (side1_x, queue); \
      REDEN (x); \
      rotate_side2 (x); \
      COMMON_CASE (side2 (parent_node), side2, rotate_side1); \
      break; \
    } \
  REDEN (x); \
  node = parent_node; \
  parent_node = PARENT (parent_node); \
}

#define COMMON_CASE(node,side2,rotate_side1) \
do { \
     NODETYPE* y = node; \
     COPY_COLOR (y, parent_node); \
     BLACKEN (parent_node, queue); \
     BLACKEN (side2 (y), queue); \
     rotate_side1 (parent_node); \
     BLACKEN (LEFT (CAST(NODETYPE*,queue)), queue); \
   } while (0)

  for (;;)
    {
      //  cout << "[2]";////////////////
      if (parent_node == CAST(NODETYPE*,queue) || IS_RED (node))
        {
          BLACKEN (node, queue);
          break;
        }
      if (node == LEFT (parent_node))
        REMOVE_BODY (LEFT, ROTATE_LEFT, RIGHT, ROTATE_RIGHT) /* no ; needed */
      else
        REMOVE_BODY (RIGHT, ROTATE_RIGHT, LEFT, ROTATE_LEFT) /* no ; needed */
    }

 rbtree_is_balanced:

  PARENT_SET (CAST(NODETYPE*,queue), CAST(NODETYPE*,queue));
}

#endif


/*---------------------------------------------------------------------------*/

