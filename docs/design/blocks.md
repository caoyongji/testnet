# Block

The following describes how things are meant to be, and maps to how
things are in the Erlang codebase.  Items described here and known not
to be reflected yet in the codebase are noted with a *TO-BE* prefix.

## Glossary

["Merkle tree"](https://en.wikipedia.org/wiki/Merkle_tree).

"Leaf datum", in the context of a Merkle tree: value of the leaf.

"Leaf proof", in the context of a Merkle tree: list of Merkle tree
nodes from the leaf to the root proving the inclusion of the chosen
leaf datum.

["State Merkle trees"](/docs/design/trees.md).

["Transaction"](/docs/design/transaction_types.md).

## Block-related data structures exchanged on the network

The following describes the block-related data structures as exchanged
among peers in the network.  It also maps, where possible, items in
such network-exchanged data structures to items in the internal
representation of data structures in the Erlang codebase.

### Block

The block includes:
* [Block header](#block-header);
* Ordered representation of transactions;
  ```erlang
  #block.txs
  ```
  * The first transaction is a `coinbase` transaction;
  * No transaction after the first can be a `coinbase` transaction;
    * *TO-BE The codebase does not check that no `coinbase` transaction is present after the first.  This shall be checked.*
  * Each transaction has objective constraints on its presence in the
    block.  For details on such criteria, please refer to the Erlang
    code implementing transactions (files `*_tx.erl`).
* Partial representation of state Merkle trees (i.e. accounts,
  channels, proof of existence, proof of burn and oracles) before
  application of transactions, specifically proof and datum of each
  leaf affected by at least one transaction in the block.
  ```erlang
  #block.proofs
  ```
  * Explicating further the description above:
    * Each block does not necessarily contain proof and datum of every
      leaf;
    * The datum of any leaves not affected by at least one transaction
      in the block cannot be determined only from the information in
      the block, neither can the presence or the proof of most leaves
      not affected by at least one transaction in the block be
      determined only from the information in the block.
  * *TO-BE The codebase holds a place but does not use such item.  The item shall be used.*

See also [aeternity whitepaper] subsection "II-A.4) Block contents".

### Block header

The block header includes:
* Hash of previous block header;
  ```erlang
  #header.prev_hash
  #block.prev_hash
  ```
* Height;
  ```erlang
  #header.height
  #block.height
  ```
* Protocol version;
  ```erlang
  #header.version
  #block.version
  ```
* Hash summarizing partial representation of state Merkle trees before
  application of transactions (in block - `#block.proofs`), and
  transactions (in block - `#block.txs`);
  ```erlang
  #header.txs_proof_hash
  ```
* Hash summarizing all state Merkle trees (i.e. accounts, channels,
  proof of existence, proof of burn and oracles) as after application
  of transactions;
  ```erlang
  #header.trees_hash
  #block.trees_hash
  ```
* Approximate timestamp of mining;
  ```erlang
  #header.time
  #block.time
  ```
* Difficulty;
  ```erlang
  #header.difficulty
  #block.difficulty
  ```
  * The computation of difficulty is described
    [here](/docs/design/difficulty.md).
* Proof of work.
  ```erlang
  #header.nonce
  #block.nonce
  ```
  * A proof of work depends on the whole block header and is
    parametrized by the difficulty.

The block header `H` is serialized as per function
`headers:serialize/1`:

```erlang
HB = 32*8,
HtB = 32,
TB = 32,
VB = 16,
DB = 16,
<<(H#header.prev_hash):HB/bitstring,
  (H#header.height):HtB,
  (H#header.time):TB,
  (H#header.version):VB,
  (H#header.trees_hash):HB/bitstring,
  (H#header.txs_proof_hash):HB/bistring,
  (H#header.difficulty):DB,
  (H#header.nonce):HB
>>.
```

See also [aeternity whitepaper] subsection "II-E.2) Light clients".

### Genesis block

The genesis block is a special block whose accounts state Merkle tree
consists of one account with key `0` and balance the initial amount of
tokens.  See `block:genesis_maker/0`.

## Node-local model of blocks

The following describes the node-local internal representation of
blocks, as per Erlang codebase, both from a static and a dynamic point
of view.

### Node-local block-related structures

The internal representation of each observed valid block is persisted
as `blocks/<HashOfBlockHeaderInBase58>.db` - see `block:save/1`.

The header of the most recent block in the local view of the block
chain is kept in memory by process `headers` - see field `top` of the
state of the `headers` process, in module `headers`.

Some block headers, including at least the header of the most recent
block in the local view of the block chain, are kept in memory by
process `headers` - see field `headers` of the state of the `headers`
process, in module `headers` - in a dictionary having as key the hash
of the block header and as value the block header.

The hash of each observed header - both valid and invalid - is
persisted, as part of a set of such hashes, as file
`data/block_hashes.db` - see `block_hashes:save/1`.  Such set of
hashes is also kept in memory in process `block_hashes`, that at its
initialization attempts to restore its in-memory copy from the
persisted copy.

The observed valid block headers are persisted as file
`data/headers.db`.  Such headers are appended to the file in order of
observation.

### Node-local block-related workflows

#### Initialization of `tx_pool`

At startup, the node starts the `tx_pool` process, that at its
initialization:
* Computes the internal representation of the genesis block;
* Persists (`block:save/1`) the genesis block, by hash of its header;
* Synchronously resets the state of process `headers`
  (`headers:hard_set_top/1`) to the genesis block;
* Delegates asynchronously process `block_hashes`
  (`block_hashes:save/1`) to persist, keeping a copy in memory, the
  hash of the header of the genesis block.

#### Block absorption following successfully mined block

The node mines:
* Continuously - as process `mine` as defined in module `mine`; and
* At request received on the internal API at path `/v1/mine-block`,
  that invokes Erlang function `api:mine_block/2` - as the process
  handling the HTTP request.

Both entry points for mining invoke `block:mine/2`.  After
successfully mining a block, the process absorbs the block.

When absorbing the block:

* (See `headers:absorb/1` for the implementation.) If process
  `headers` does not have yet the block header, the process absorbing
  the block:
  * Validates the header;
  * Requests synchronously process `headers` (see
    `headers:handle_call` with request `{add, Hash, Header}`) to add
    the header to its collection of headers, that in turn:
    * Unconditionally includes such header in its collection of
      headers;
    * Depending on `#header.accumulative_difficulty`, it considers
      such header as the most recent one in its local view of the
      block chain.
  * Persists the header in the sequence of observed valid block
    headers.

* (See `block_absorber:save/1` for the implementation.) The process
  absorbing the block requests synchronously process `block_absorber`
  to absorb the block, that in turn:
  * If the block header has already been observed, ignores the block;
  * If the block header has not been observed yet:
    * Checks that the block before such block (1) has a header that
      has been observed and (2) is a block that is both observed and
      valid;
    * Delegates asynchronously process `block_hashes`
      (`block_hashes:save/1`) to persist, keeping a copy in memory,
      the hash of the header;
    * (Calls `headers:absorb/1` - again;)
    * Checks the block (`B`), and computes the internal representation
      of the block `B2` with the internal representation of the state
      trees;
    * Persists (`block:save/1`) block `B2`, by hash of its header;
    * (Asserts that the hash of the block header of `B2` is the same
      as the one of `B`;)
    * Sleeps for 100 ms;
    * As a sequence of three distinct non-atomic operations:
      * Retrieves pending transactions (`tx_pool:data/0`);
      * Synchronously requests process `tx_pool` to reset its state,
        that involves:
        * Contacting process `headers` for the most recent block
          header;
        * Identifying the most recent available observed valid block
          in the chain identified by such most recent block header
          retrieved from process `headers`.
      * Asynchronously feeds - one by one - the previously retrieved
        pending transactions to the `tx_pool` process.

#### Block absorption following network interactions

Block absorption following network interactions is similar to block
absorption following successfully mined block.  See
`block_absorber:save/1` and its asynchronous counterpart
`block_absorber:enqueue/1`.

## References

[aeternity whitepaper]: https://blockchain.aeternity.com/%C3%A6ternity-blockchain-whitepaper.pdf

## TODO

Assuming that transactions in block are meant to be able to be applied in arbitrary order on the state Merkle trees, both when making the block header to-be-mined and when verifying the block, explicate constraints on transactions in block. E.g. can a block include for the same account a transaction crediting it and another debiting it - where the debiting transaction would make the account balance negative if applied before the crediting one?

Confirm from the codebase that `#leaf.meta` (as opposed to `#leaf.value`) is not meant to be exchanged on the network as part of the data structures described in this document, then reconsider using the term "value" rather than "datum" for leaves of Merkle trees.

Detail Merkle tree and Merkle proof.

Include `#header.accumulative_difficulty` in this description.

Decide whether to include `#block.comment` in this description.

Confirm exclusion of `#block.prev_hashes` from this description.

Clarify why governance tree is not considered for computation of hash `#header.trees_hash`.

Clarify appended-to-but-not-read-from file `data/headers.db`.

Clarify multiple calls to `headers:absorb/1`.

Clarify why sleeping 100 ms after absorbing block.
