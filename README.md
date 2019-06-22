# Loopbreaker

TODO: general description

@isovector:
> As described in [Writing Custom Optimization Passes](https://reasonablypolymorphic.com/blog/writing-custom-optimizations/),
The `polysemy-plugin` has had support for creating explicit loopbreakers for
self-recursive functions. The result is pretty dramatic code improvements in a
lot of cases when `-O2` is turned on.

> Rather embarrassingly, after publishing that post, it turned out that my
implementation didn't in fact improve optimizations. Sure, it spit out the
correct code, but it was being done too late, and some special analysis passes
had already run. The result: we'd generate loopbreakers, but they wouldn't be
used.

> [TheMatten](https://github.org/TheMatten) took it upon himself to fix this.
There's no trick --- just do the same transformations after renaming, rather
than after typechecking. We realized this plugin is useful outside of
polysemy, so it's been released as a standalone package.
