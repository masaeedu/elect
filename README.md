Suppose an electorate is voting on some proposal. Suppose also that the space of possible votes `Vote : Set` that can be cast carries some structure.

- `Vote : Set` forms a partial order under the relation of "acceptability". `A ≤ B` implies that a `B`-voter finds `A` a (possibly less) acceptable outcome.

- `Vote` is a meet-semilattice under the operation of "consensus". If two voters cast votes `A` and `B` respectively, some consensus `A ∧ B` can be found such that:

  1. `A ∧ B` is acceptable to both voters (`A ∧ B ≤ A, A ∧ B ≤ B`)

  2. Any other consensus `A ∧' B` is more odious to them `A ∧ B` (`A ∧' B < A ∧ B`).

  3. consensus is commutative

- `Vote` has an upper-bound, which is to "abstain". A voter who abstains will accept any outcome. Finding consensus with an abstain voter is easy, because they'll give you whatever you want.

- `Vote` is a join-semilattice under the operation of "compromise". If two voters cast votes `A` and `B` respectively, some compromise `A ∨ B` can be devised by adding ["pork"](https://en.wikipedia.org/wiki/Pork_barrel) to `A` or `B` such that:

  1. An `A ∨ B`-voter would find both `A` and `B` acceptable
     - NB: neither of the `A`, `B` voters need find `AB` acceptable, since the other side has stuffed the proposal with their unacceptable "pork"

  2. `A ∨ B` is the least acceptable solution that would satisfy a voter of any other compromise `A ∨' B` (`A ∨ B < A ∨' B`).
     - NB: a good compromise dissatisfies both parties. this means it can't have "pork" for one party, without having it be balanced out by "pork" for the other, each dissatisfying the other.

  3. compromise is commutative

- `Vote` has a lower-bound, which is a "veto". A veto-er does not find any outcome besides a veto acceptable. Any voter must be prepared to accept a veto (`∀ V : Vote | Veto ≤ V`).

  - A consensus involving a veto absorbs any other vote

  - The only sensible compromise with a veto-er is to do whatever you were going to (they don't like anything on the table, so you can't make them happier by adding "pork")

- `Vote` is a distributive lattice. Given three votes `A`, `B`, `C`, the consensus of `A` with a compromise between `B` and `C` is the same as a compromise between the consensus of `A` and `B` and the consensus of `A` and `C` (`A ∧ (B ∨ C) = (A ∧ B) ∨ (A ∧ C)`)

  - `A ∧ (B ∨ C) ≤ (A ∧ B) ∨ (A ∧ C)`. TODO: justify this
  - `(A ∧ B) ∨ (A ∧ C) ≤ A ∧ (B ∨ C)`. TODO: justify this

- A distributive lattice is a semiring.
