# Notes

General development notes I might one day forget.

## Should Step declare it uses N threads to compile

No. The reason is that the first prepare (which basically runs on its own, or only while other things are winding down) does take N threads. Any subsequent prepares (e.g. for the bisection) only require 1 thread. You would mostly end up delaying bisections for a time that doesn't really happen.

## Should you be able to discover more tests while running

Yes, theoretically that seems reasonable. In practice, it's not necessary, especially with something like Step. If you were to do it,  you'd keep a set of tests that are required and let each additional test add to it. You'd still do dependencies as now.
