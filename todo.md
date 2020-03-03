# TODO

### Make the lexer an object.

Create a lexer struct akin to

```rust
struct {
    buffer: peekable pointer
    state: enum {
        Default
        Comment
        EatingNumber
        EatingIdentifier
    }

    pub fn new(buffer) Lexer {}

    pub fn next() Token {
        loop over the buffer {
            switch State {
                State 1 {
                    match {
                        A => B
                        B => { C; break; }
                    }
                }
                State 2 {
                    match {
                        A => B
                        B => { C; break; }
                    }
                }
            }
        }
    }
}
```

Then the parser would run `Lexer.next()` on demand.

### Implement more binary ops

```
Modulo %
Bitwise AND &
Bitwise OR |
Bitwise XOR ^
Bitwise shift left <<
Bitwise shift right >>
```

### x86_64 Optimizations

- Use `xor rax, rax` in place of `mov rax, 0`.
- https://graphics.stanford.edu/~seander/bithacks.html
- https://bits.stephan-brumme.com/