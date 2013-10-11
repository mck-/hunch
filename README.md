Hunch
===========

Simple boilerplate code to build an API-only on Hunchentoot. Includes some useful tools to deal with JSON input and output. Also features a dot-notation alist accessor (for those missing the easy JSON accessors).

```
{
    "hello": {
        "key": {
            "key2": "world"
        }
    }
}

Decoding the above JSON object with CL-JSON results into the following alist:

HUNCH> (defvar obj '((:HELLO (:KEY (:KEY-2 . "world")))))
HUNCH> @obj.hello.key.key-2
"world"
```

Also includes a literal JSON creation syntax:

```
  (with-json-response
    (encode-json-to-string
     {:http-code "200"
      :status "API is running!"})))
```

See [routes](https://github.com/mck-/hunch/blob/master/routes.lisp) for examples.


Build
--------

Can be build with `make`, which will build a `build` binary in the current directory, and can be run as a daemon:

```
➜  $ make
buildapp --output build --asdf-tree ~/quicklisp/dists/quicklisp/software/ --asdf-path /Users/mck-/Lisp/hunch --load-system hunch --entry hunch:main
;; loading system "hunch"
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into build:
writing 5968 bytes from the read-only space at 0x0x20000000
writing 5248 bytes from the static space at 0x0x20100000
writing 69828608 bytes from the dynamic space at 0x0x1000000000
kdone]
➜  $ ./build 3005
Starting API on port 3005
---=================================---
Server log output going to: "server.log"

To test if the API is running, curl: http://localhost:3005/
```