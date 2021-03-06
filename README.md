[![Build Status](https://travis-ci.org/michalrus/hstatusbar.svg?branch=master)](https://travis-ci.org/michalrus/hstatusbar)

# hstatusbar

## Why

If you don’t like Bash, here’s a more principled way to define input for [lemonbar](https://github.com/LemonBoy/bar) or similar.

Also, it feels crisper. Modules that reflect some events, do that in “real-time,” with very little delay. Time is synced to ± 1.5 ms, etc.

## How to install

The easiest way I know:

1. Install [Nix](https://nixos.org/nix/).
1. Clone the project.
1. Run `nix-build`.
1. The executable will be in `./result/bin/hstatusbar`.

If you want to take some other route, keep in mind that dependencies in the `.cabal` file don’t have pinned versions, as these are based on versions available in a particular SHA1 of Nixpkgs.

## Examples

1. A basic one:

   ```
   $ hstatusbar 'time is $(time.local "%a %d %b %H:%M:%S %Z") • $(time.universal "%H:%M %Z")'
   time is Sat 22 Apr 02:55:30 CEST • 00:55 UTC
   time is Sat 22 Apr 02:55:31 CEST • 00:55 UTC
   time is Sat 22 Apr 02:55:32 CEST • 00:55 UTC
   …
   ```

2. *TODO: my screenshot*

## Available interpolators

* `time.local <fmt>`
* `time.universal <fmt>`
* `cpu <temp_path>` — temperature & load average.
* `memory` — used RAM and swap.
* `disk [<path0> …]` — available disk space for paths.
* `battery <uevent_path> <icon_charging> <icon_full> <icon_discharging> <warn_capacity> <warn_fmt1> <warn_fmt2>` — when capacity falls below `<warn_capacity>`%, the status will start blinking, alternating between `<warn_fmt1>` and `<warn_fmt2>`, with `%s` in them being the displayed text status.
* `alsa.volume <fmt_muted> <fmt_normal>` — ALSA volume level with `%v` as the level in `<fmt>`s.
* `xtitle <max_length>` — uses [xtitle](https://github.com/baskerville/xtitle) to get current window title.
* `bspwm <normal_fmt> <selected_fmt> <urgent_fmt> [<icon0> …]` — uses [Bspwm](https://github.com/baskerville/bspwm)’s `bspc subscribe` to get the status of workspaces. (*Note: I’ve only ever used a single screen, so a PR might be needed to get this working sensibly on multiple screens.*)

## To-do

1. Use [Dhall](https://github.com/Gabriel439/Haskell-Dhall-Library) for configuration instead of the crappy format strings.
