[![Build Status](https://travis-ci.org/michalrus/hstatusbar.svg?branch=master)](https://travis-ci.org/michalrus/hstatusbar)

# hstatusbar

## Examples

```
$ hstatusbar 'time is $(time.local "%a %d %b %H:%M:%S %Z") • $(time.universal "%H:%M %Z")'
time is Sat 22 Apr 02:55:30 CEST • 00:55 UTC
time is Sat 22 Apr 02:55:31 CEST • 00:55 UTC
time is Sat 22 Apr 02:55:32 CEST • 00:55 UTC
…
```
