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

## Development

```bash
cargo run -- repl examples/web/websockets.kom
```

![REPL](/docs/repl-screenshot-2025-04-21-001.png?raw=true)

### Tests

To run the regression tests (golden tests) for the parser, use the following command:

```bash
cargo test golden --package komrad-parser -- --show-output
```
