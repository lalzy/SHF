The first attempt at an engine built atop of SDL. It originally was **NOT** an engine, instead started as a series of helpers for SDL to make things quicker.

My [space-invaders](https://github.com/lalzy/Spaceinvaders-clone) test was a big test-case for building this engine.

it was depricated in favor of [yggdrasil](https://github.com/lalzy/yggdrasil-old) and exist as personal journey/history only. Much of the core concepts of what started SHF retains in Yggdrasil, but was rewritten to be clearer (code-wise), and feel more like an engine, instead of a series of SDL-helpers.

One of the issues of SHF (SDL Helper Functions), is how tied it is to SDL, which made changing renderer(and 'future' ambition) not realistic, and so Yggdrasil was born, to replace this.


As for Yggdrasil linked above, it too is deprecated (see it's own page for details), but to give some context. Not all of SHF's features, are found within Yggdrasil (such as the CFFI error system, or the compile 'overwrite'), as the goal was to make it capable of producing a game using only engine-features.
