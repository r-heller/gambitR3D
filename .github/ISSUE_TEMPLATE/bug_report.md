---
name: Bug report
about: Something in the rules engine, app, analysis or training does not behave as documented
---

**What happened**
A short description, and what you expected instead.

**Reproduction**
The smallest R code or app steps that show it. For a position, attach the game or position file
(`g3_write_game()` / `g3_write_position()`); please do not attach private game collections.

**Rules profile**
Output of `gambitR3D::g3_rules_profile()$rules_hash`.

**Session**
Output of `utils::sessionInfo()`, and for training problems the report of `gambitR3D::g3_setup_ml()`.
