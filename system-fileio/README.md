## system-fileio

Legacy package (deprecated).

This is a small wrapper around the `directory`, `unix`, and `Win32`
packages, for use with `system-filepath`. It provides a consistent API
to the various versions of these packages distributed with different
versions of GHC.

In particular, this library supports working with POSIX files that have
paths which can't be decoded in the current locale encoding.
