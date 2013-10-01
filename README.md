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


Build
--------

Can be build with `make` and run as a daemon.