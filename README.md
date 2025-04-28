# Vii

Vii is a Vietnamese input method engine.

## Features

* Supports Telex and VNI input methods
* Integrates with `ibus`
* Tested on GNOME + Wayland

## Build

* Install Haskell via [GHCup](https://www.haskell.org/ghcup/)
* Clone the repository:
  ```
  git clone https://github.com/7c78/vii.git
  cd vii
  ```
* Build with Cabal:
  ```
  cabal build
  ```

## Install

* Locate the built binary, typically:
  ```
  dist-newstyle/build/x86_64-linux/ghc-9.6.6/vii-0.0.1/x/vii/build/vii/
  ```
* Install the engine:
  ```
  sudo ./vii install 
  ```
* Restart `ibus`:
  ```
  ibus restart
  ```
* Add the Vii input engine:
  ```
  Settings > Keyboard > Add Input Source > Vietnamese > Vietnamese (Vii) > Add
  ```
