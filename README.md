# Loopbreaker

Performance of libraries like [polysemy](https://github.com/polysemy-research/polysemy)
depends on code being aggresively inlined. Problem is that GHC is not very
keen on inlining self-recursive definitions. Luckily, there's a way we can
trick compiler to do so: by introducing intermediate _loopbreaker_ that simply
calls our original function:
```
fact :: Int -> Int
fact 0 = 1
fact n = n * fact' (n - 1)
{-# INLINE fact #-}

fact' :: Int -> Int
fact' = fact
{-# NOINLINE fact' #-}
```
But because this is ugly boilerplate nobody wants to write, we created GHC
plugin that searches for such recursive definitions and automatically inserts
loopbreakers during compilation.

To quote [isovector](https://github.com/isovector):
> As described in [Writing Custom Optimization Passes](https://reasonablypolymorphic.com/blog/writing-custom-optimizations/),
The `polysemy-plugin` has had support for creating explicit loopbreakers for
self-recursive functions. The result is pretty dramatic code improvements in a
lot of cases when `-O2` is turned on.

> Rather embarrassingly, after publishing that post, it turned out that my
implementation didn't in fact improve optimizations. Sure, it spit out the
correct code, but it was being done too late, and some special analysis passes
had already run. The result: we'd generate loopbreakers, but they wouldn't be
used.

> [TheMatten](https://github.com/TheMatten) took it upon himself to fix this.
There's no trick --- just do the same transformations after renaming, rather
than after typechecking. We realized this plugin is useful outside of
polysemy, so it's been released as a standalone package.
