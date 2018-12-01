# B+ Trees

Indexes in Noblit are log-structured merge trees of B+ trees. The B+ trees have
the same on-disk format as memory format, which allows them to be memory mapped.

    L0: | k0 ..                                                            kn |
    L1: | k0 ..                    k(n/2) | k(n/2) ..                      kn |
    L2: | k0 .. k(n/4) | k(n/4) .. k(n/2) | k(n/2) .. k(3n/4) | k(3n/4) .. kn |

When L0 grows too big: take either range k0..k(n/2), or k(n/2)..kn, and merge
with the tree at the lower level. The child to merge into is the lower-level tree
with the least children. This may overflow the lower level, in which case we
recurse.

This looks an awful lot like a tree of trees. What if we go with the B+ tree
with payload after all?

    Node | child range keys | self-values |

Flush: push all self-values down to a lower level. Or maybe only part of them.
Then accept the new values. How to keep the balance?

Alternative: keep only one tree at every level. When tree at level L grows
larger than a threshold, rewrite tree at L+1 to be the merge.

  L0: B keys
  L1: B^2 keys
  L2: B^3 keys
  ...
  Lk: B^k keys

When L0 overflows, need to rewrite B^2 keys. How often does L0 overflow? Once
every B inserts. Say every insert is cost 1 for insertion in L0, then once in
B we pay B^2 writes -> Cost 1 + B per write. Then once in B^2 keys, we overflow
L1, and we need to rewrite the B^3 keys in L3. So additional cost of B^3/B^2 =
B. Etc. So the cost per write is 1 + B*levels, and levels = log_B(keys). So we
maintain log(n) insert cost. And the tree at each level is trivially balanced,
because when we rewrite it, we know its size, and the inputs are sorted, so we
can write a balanced tree in one go.

Query cost: log_B(B) + log_B(B^2) + log_B(B^3) + ... + Log_B(B^k) = k^2/2.

For k levels, where k is roughly log_B(keys), we pay k^2/2 query cost. That is
worse than log_B(keys). Not good!
