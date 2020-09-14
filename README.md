# In-memory key/value store
- CLI application that runs in "god" mode
- all keys and values are strings

Commands:
- `put <key> <value>` adds a value to the store (no spaces allowed)
- `put` overrides existing values
- `get <key>` to read the value at key
- `dump` to show all keys and values
- `exit` to exit

---

- [x] Commands
- [x] Interpret commands
- [x] Parse from string to Command
- [x] Console in/out
