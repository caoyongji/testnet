# Blockchain difficulty

- Initial difficulty comes from `constants:initial_difficulty`.

- How many blocks until we recalculate difficulty is in `constants:retarget_frequency`.

- `block:make` will use difficulty calculated by `headers:difficulty_should_be` which adjusts according to retarget frequency.

- Mine difficulty (adjusted using block reward) in `block:mine2` is used to calculate the nonce for the mined block.

# The difficulty oracle

# Questions

Q: Why is difficulty for a new oracle half the previous one in `api:new_question_oracle`?

