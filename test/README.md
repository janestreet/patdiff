---
title: Patdiff test directory layout
uuid: f4043791-b602-3b04-6282-ff80a2cc215f
parent: ../README.md
---

patdiff tests are separated into two directories, `src` and
`feedback`.

The goal is to be clear when a test changed because we improved the
diff vs. when a test changed because we broke something.

- `test/src` contains tests that document expected behavior.
- `test/feedback` contains tests that document unexpected behavior,
  which we would like to change (and then move to test/src).
- `test/bin` contains some common code, such as unified test setup
  scripts.
