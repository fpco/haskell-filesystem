# Changelog for system-fileio

## 0.3.16.6

* Fix building on Windows ([issue #31](https://github.com/fpco/haskell-filesystem/issues/31), regression in 0.3.16.5).
* Tested with:
  - Cabal and GHC 8.0 - 9.12.0 on Linux
  - Stack and GHC 9.8.2 on Linux, macOS and Windows.

## 0.3.16.5

* Fix building of the testsuite with `unix-2.8`.
* Tested with GHC 8.0 - 9.12.0.

## 0.3.16.4

* Fix for Win32 2.6 and above [#21](https://github.com/fpco/haskell-filesystem/pull/21).

## 0.3.16.2

* `withHANDLE` (Win32) now works on directories [#8](https://github.com/fpco/haskell-filesystem/issues/8) [#10](https://github.com/fpco/haskell-filesystem/pull/10).

## 0.3.16.1

* Use different path encoding on Darwin in POSIX tests [#6](https://github.com/fpco/haskell-filesystem/pull/6).

## 0.3.16

Maintenance taken over by FP Complete.
