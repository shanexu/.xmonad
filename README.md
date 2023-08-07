# xmonad.hs

## install dependencies

```sh
cabal update
cabal install xmonad --bin --global --overwrite-policy=always
cabal install xmonad --lib --global
cabal install xmonad-contrib --lib --global
cabal install xmonad-dbus --bin --global --overwrite-policy=always
cabal install xmonad-dbus --lib --global
cabal install dbus --lib --global
cabal install hostname --lib --global
cabal install X11 --lib --global
```

## compile

```sh
xmonad --recompile
ghc xmonadctl.hs
```

## key binding


