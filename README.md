[![Build Status](https://travis-ci.org/michalrus/hstatusbar.svg?branch=master)](https://travis-ci.org/michalrus/hstatusbar)

# hstatusbar

## Why

If you don’t like Bash, here’s a more principled way to define input for [lemonbar](https://github.com/LemonBoy/bar) or similar.

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
* `memory` — used RAM and swap.
* `xtitle <max_length>` — uses [xtitle](https://github.com/baskerville/xtitle) to get current window title.
* `bspwm <normal_pre> <normal_post> <selected_pre> <selected_post> <urgent_pre> <urgent_post> [<icon0> …]` — uses [Bspwm](https://github.com/baskerville/bspwm)’s `bspc subscribe` to get the status of workspaces. (*Note: I’ve only ever used a single screen, so a simple PR might be needed to get this working sensibly on multiple screens.*)
