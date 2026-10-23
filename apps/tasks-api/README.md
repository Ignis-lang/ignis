# tasks-api

A small task-tracker web service under `apps/`, structured as a layered
(hexagonal) Ignis project. It is the reference for how `std::web`
applications can be organized when they outgrow a single file: the domain
and the application never touch HTTP, and only the adapter layer knows
about statuses. It serves HTTP when run, so CI builds it and runs its
in-process test suite instead of running the server.

## Layers

## Layers

```
src/
  main.ign                    composition root: the only file that names the
                              concrete repository (MemoryTaskRepository)
  app.ign                     TasksApi<R>: the Web::App record; owns the
                              shared state, builds the router per thread
  domain/                     the entity, its invariants, the repository
    task.ign                  trait, and domain errors — imports neither
    errors.ign                std::web nor std::http, no status codes
    repository.ign
  application/                use cases, generic over <R: TaskRepository>;
    tasks.ign                 validation and the ownership rule live here
  infra/
    memory_repository.ign     the port implemented over Shared<TaskTable>
  adapters/http/
    api.ign                   the contract: Api<R>, the controller and
                              filter signatures, the endpoint ids, and the
                              single TaskError -> status mapping
    router.ign                the route table and the only adapter from a
                              named function to Web::Middleware
    tasks.ign                 one named controller per /tasks endpoint
    health.ign                GET /health
    middleware.ign            request ids, logs, bearer auth, JSON rule
    json.ign                  task <-> JSON and request-body reading
  tests/
    tasks_test.ign            in-process tests through the same router
```

The dependency direction is fixed: `adapters -> application -> domain`,
with `infra` implementing the domain's `TaskRepository` port and `main`
wiring it in. Status codes appear only under `adapters/http/`
(NOT_FOUND -> 404, VALIDATION -> 422, FORBIDDEN -> 403,
BAD_REQUEST -> 400).

One adapter detail: `Web::Middleware` is a closure type and a closure is
not `Send` or `Sync`, so one adapter per route captures the shared state
once (`adapters/http/router.ign`). Until the compiler can take the value
of a generic function, the controller also travels as an enum that a
dispatch function matches on; when `fn(A) -> R` reaches the official
stage0, the adapter can take the function pointer directly and the
dispatch goes away.

## Run

```bash
ignis build -o tasks-api && ./tasks-api
```

The server listens on `127.0.0.1:8080` with bearer token `secret`.

```bash
curl -s http://127.0.0.1:8080/health
curl -s -H "Authorization: Bearer secret" -H "X-User: ana" http://127.0.0.1:8080/tasks
curl -s -H "Authorization: Bearer secret" -H "X-User: ana" \
  -H "Content-Type: application/json" -d '{"title": "Write docs"}' \
  http://127.0.0.1:8080/tasks
curl -s -H "Authorization: Bearer secret" -H "X-User: ana" \
  -H "Content-Type: application/json" -d '{"done": true}' \
  -X PATCH http://127.0.0.1:8080/tasks/1
curl -s -H "Authorization: Bearer secret" -H "X-User: ana" \
  -X DELETE http://127.0.0.1:8080/tasks/1
```

Every /tasks request needs `Authorization: Bearer secret` and an
`X-User` name; the owner of a task is the `X-User` who created it, and
only the owner may change or delete it. Responses carry an
`X-Request-Id` header.

## Test

```bash
ignis test
```

The tests drive the same router the server threads use, without a
socket; this is the suite CI runs, since the server itself never
terminates and is therefore not run in CI.

The layering also makes the example an architecture slot: swap
`infra/` for a database-backed `TaskRepository` and nothing in
`application/` or `domain/` changes (microservice), publish
`application/` as the library behind a queue consumer instead of HTTP
and the adapters change but the use cases do not (serverless/hexagonal).
