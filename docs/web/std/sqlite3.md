---
title: SQLite
description: Using std::sqlite3 for raw SQL — connections, prepared statements, transactions, errors and threading.
section: std
order: 5
status: stable
---

`std::sqlite3` is a thin, faithful binding to SQLite. The C handles `sqlite3*` and `sqlite3_stmt*`
are wrapped in types that clean up after themselves, and every failure comes back as a `Result`.
The module panics in exactly one place: `SharedConnection::new` aborts when the linked SQLite was
built with `SQLITE_THREADSAFE=0`.

If you want records mapped to tables, use `std::orm`, which is built on top of this module. If you
just want to run SQL, this is all you need.

| Type | What it is |
| --- | --- |
| `Sqlite3::Connection` | One open database. Closes on drop |
| `Sqlite3::Statement` | One compiled SQL statement. Finalizes on drop |
| `Sqlite3::Step` | `ROW` or `DONE`, what `Statement::step` reports |
| `Sqlite3::Transaction` | A scoped transaction. Rolls back on drop unless committed |
| `Sqlite3::Error` | `code` (SQLite's extended result code) and `message` |

## Requirements

The module links `libsqlite3` and includes `sqlite3.h`, so every machine that builds the compiler or
the standard library needs the SQLite development headers: `libsqlite3-dev` on Debian and Ubuntu, or
`pkgs.sqlite` in the repository's Nix dev shell.

## A first program

```ignis
import Sqlite3 from "std::sqlite3";

function main(): i32 {
    let mut connection: Sqlite3::Connection = Sqlite3::Connection::openInMemory().unwrap();

    connection.exec("CREATE TABLE tasks (id INTEGER PRIMARY KEY, title TEXT NOT NULL)").unwrap();

    let mut statement: Sqlite3::Statement =
        connection.prepare("INSERT INTO tasks (title) VALUES (?)").unwrap();

    statement.bindText(1, "write the ORM").unwrap();
    statement.step().unwrap();

    return connection.lastInsertRowId() == 1 ? 0 : 1;
}
```

## Connections

| Method | Description |
| --- | --- |
| `Connection::open(path)` | Opens or creates the database file `path` |
| `Connection::openInMemory()` | Opens a private `:memory:` database |
| `close()` | Closes early and reports the result. Dropping the connection does the same silently |
| `exec(sql)` | Runs one or more statements separated by semicolons, with no parameters and no rows read |
| `prepare(sql)` | Compiles one statement into a `Statement` |
| `lastInsertRowId()` | The row id of the last insert |
| `changes()` | How many rows the last statement changed |
| `busyTimeout(milliseconds)` | How long to wait when the database is locked |
| `begin()` | Starts a transaction: `Result<Transaction, Error>` |

`open` runs `PRAGMA journal_mode=WAL` on file databases so readers do not block behind a writer. If
the filesystem cannot support WAL, `open` closes the handle and returns the pragma's error; it does
not fall back to the previous journal mode. `:memory:` databases skip the pragma, and only the
exact path `":memory:"` counts as one.

## Statements

`prepare` compiles **only the first statement** of its SQL. Anything after the first semicolon is
silently dropped. Run each statement through its own `prepare`, or hand multi-statement scripts that
need no parameters and return no rows to `exec`.

**Binding.** Parameter indexes are **1-based**, as in SQLite's C API (`?1` is index 1).

| Method | Binds |
| --- | --- |
| `bindNull(index)` | `NULL` |
| `bindInt(index, value: i64)` | An integer |
| `bindReal(index, value: f64)` | A floating-point number |
| `bindText(index, text: str)` | Text. SQLite copies the bytes at bind time, so your buffer can go away before `step` |
| `bindBlob(index, &bytes)` | Bytes, copied the same way |

**Stepping.** `step()` returns `Result<Step, Error>`: `Step::ROW` for each row and `Step::DONE` when
the statement has finished. Anything else is an `Sqlite3::Error`.

**Reading.** Column indexes are **0-based**.

| Method | Reads |
| --- | --- |
| `columnCount()` | The number of columns |
| `columnIsNull(column)` | Whether the value is `NULL` |
| `columnInt(column)` | An `i64` |
| `columnReal(column)` | An `f64` |
| `columnText(column)` | A `String` |
| `columnBlob(column)` | A `Vector<u8>` |

The getters coerce values the way SQLite does, so `columnInt` on a `TEXT` column parses a number.
Use `columnIsNull` to tell a real `NULL` apart from a coerced `0` or empty string.

**Reuse.** `reset()` rewinds a statement so it can run again. Bindings survive the reset — nothing
is cleared — so a parameter you do not bind again keeps its previous value; bind every parameter
before stepping. A statement stays compiled, so a query that runs many times is prepared once.
After an error, reset the statement before using it again.

## Transactions

`connection.begin()` returns `Result<Sqlite3::Transaction, Sqlite3::Error>`. Call `commit()` to keep
the changes or `rollback()` to discard them. Both return `Result<void, Error>`.

If the transaction is dropped without `commit`, it rolls back. An early `return` or an error path
therefore cannot leave a transaction open. That rollback on drop is silent and best effort; code
that needs to know the outcome should call `commit` or `rollback` itself.

- Calling `commit` or `rollback` a second time is a misuse error, not a panic.
- A failed `commit` leaves the transaction unfinished, so drop still cleans up.
- A transaction does not own its connection. Using it after `close()` gives a misuse error rather
  than touching freed memory, but it cannot outlive the `Connection` value itself. Declaring the
  connection first handles this, because it then drops last.

## Errors

Every fallible operation returns `Result<T, Sqlite3::Error>`. `Error.code` is SQLite's extended
result code and `Error.message` is what `sqlite3_errmsg` reports for the connection.

`isConstraint()` is true for the `SQLITE_CONSTRAINT` family: primary key, unique, not-null, check and
foreign-key violations. It tests the primary part of the code (`code & 0xff`), so every specific
constraint code matches. A service typically turns constraint errors into HTTP 409 or 422 and every
other error into 500.

## Threads

A `Connection` holds a raw `sqlite3*`, so it is not `Send`, and a `Mutex<Connection>` does not
compile. To share one connection between threads, wrap it in `Sqlite3::SharedConnection` and put
that behind a `Mutex`:

```ignis
import Sqlite3 from "std::sqlite3";
import Arc, Mutex, MutexGuard, Shared from "std::sync";

function insertThroughShared(connection: Sqlite3::Connection): void {
  let cell: Sqlite3::SharedConnection = Sqlite3::SharedConnection::new(connection);
  let shared: Shared<Sqlite3::SharedConnection> = Arc::new<Mutex<Sqlite3::SharedConnection>>(
    Mutex::new<Sqlite3::SharedConnection>(cell)
  );

  let mut guard: MutexGuard<Sqlite3::SharedConnection> = shared.get().lock();
  guard.get().connection.exec("INSERT INTO t VALUES (1)").unwrap();
}
```

The handle can then move between threads, one at a time. A `Statement` or `Transaction` taken through
the guard must not outlive the guard.

## Not included

The binding is blocking and synchronous. There is no asynchronous API and no query builder.
