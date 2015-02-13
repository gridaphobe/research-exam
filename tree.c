#include <stdbool.h>
#include <stdlib.h>
#include <klee/klee.h>

struct node {
  int x;
  struct node * left;
  struct node * right;
};

int height(struct node * t) {
  if (t == NULL) return 0;
  int lh = t->left == NULL ? 0 : height(t->left);
  int rh = t->right == NULL ? 0 : height(t->right);
  return lh > rh ? lh : rh;
}

int abs(int x) {
  return x < 0 ? (0 - x) : x;
}

bool isBST(struct node * t) {
  if (t == NULL) return true;
  if (abs(height(t->left) - height(t->right) > 1)) return false;

  return true;
}

int main() {
  struct node * t = malloc(sizeof(struct node));
  klee_make_symbolic(t, sizeof(struct node), "t");
  klee_assume(isBST(t));
  klee_assert(0);
}
