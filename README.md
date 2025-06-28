# `komrad-lang`

**komrad is in early preview development**

Komrad is a programming language designed for writing concurrent and distributed applications.
The runtime is inspired by the actor model, and the language is agentic. It has many features
from smalltalk and lisp.

* Everything is message passing between agents
* Blocks and channels are first-class values
* An agent can be spawned from a block containing pattern-matching handlers
* Agents all the way down (e.g. `spawn Bob` sends a message to the `SpawnAgent`)

```komrad
Bob = {
    [start] {
        Io println "Hello, I'm Bob."
    }

    [message _msg] {
        Io println "Bob received message: '" + msg + "'"
    }
}

bob = spawn Bob

Alice = {
    bob = null

    [start] {
        Io println "Hello, I'm Alice."
        bob message "Hello Bob!"
    }
}

alice = spawn Alice {
    bob: bob
}
```

## Pattern Matching

Komrad's dispatch system is based on pattern matching, allowing for flexible and powerful message handling. Komrad does not have a
traditional `if` statement, but instead uses a pattern matching construct that can be used to achieve similar functionality:

```komrad
If = {
    [_(true) _{consequent}] {
        *consequent
    }

    [_(false) _{consequent}] {}

    [_(true) _{consequent} else _{alternative}] {
        *consequent
    }

    [_(false) _{consequent} else _{alternative}] {
        *alternative
    }
}

if = spawn If {}
```

### Capture Types

#### Named Capture

To capture any value in a handler pattern, use the `_name` syntax.

```komrad
Alice = {
    [see you at _time] {}
        Io println "Told Alice to meet at " + time
    }
}
```

#### Predicate Capture

To capture a value that matches a predicate, use the `_(x == 5)` syntax:

```komrad
Alice = {
    [see you at _(x == 5)] {
        Io println "Alice only meets at 5 o'clock"
    }
    
    [see you at _time] {
        Io println "Alice only meets at 5 o'clock, not " + time
    }
}
```

#### Block Capture

To capture a block, use the `_{block}` syntax. Blocks can be executed in another scope using the `*block` syntax, where `*` is also called
an "expander":

```komrad
Alice = {
    [I want you to execute _{block}] {
        Io println "Alice received a block to execute"
        *block
    }
}
```

## Websockets Example

```bash
cargo run -- repl examples/web/websockets.kom
```

![REPL](/docs/repl-screenshot-2025-04-21-001.png?raw=true)

### Tests

To run the regression tests (golden tests) for the parser, use the following command:

```bash
cargo test golden --package komrad-parser -- --show-output
```
