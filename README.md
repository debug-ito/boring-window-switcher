# boring-window-switcher

![screen shot](https://raw.githubusercontent.com/debug-ito/boring-window-switcher/master/resource/screenshot.png)

A boring window switcher for X11 desktop environments.

Unlike many other window switchers, this is NOT a daemon. This is just a regular Gtk+ application that shows windows currently open, and raises the window the user selects. It does not do any tricky keyboard grabbing.


## How to Install

To build this program, you need development files of GTK+ and Xlib. On Ubuntu, you can install them by

    $ sudo apt-get install libgtk2.0-dev libx11-dev

Then the easiest way to install this program is to use git and [stack](http://docs.haskellstack.org/en/stable/README/). Just clone this repository, build the source and install.

    $ git clone https://github.com/debug-ito/boring-window-switcher.git
    $ cd boring-window-switcher
    $ stack --install-ghc install

Building `gtk` takes long time. Be patient.

Alternatively, you can install this program directly from hackage: http://hackage.haskell.org/package/boring-window-switcher

## How to Run

    $ boring-window-switcher

## See Also

Probably you should use other feature-rich window switchers.

- [Rofi](https://davedavenport.github.io/rofi/)
- [seanpringle/simpleswitcher](https://github.com/seanpringle/simpleswitcher)
- [Frenzie/nimbler: A window switcher for users who keep a lot of windows open on multiple workspaces.](https://github.com/Frenzie/nimbler)
- [AdamCDunlap/telescope: Simple window switcher](https://github.com/AdamCDunlap/telescope)


## Author

Toshio Ito <debug.ito@gmail.com>
