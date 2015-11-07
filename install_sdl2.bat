cabal install gtk2hs-buildtools
set SDL=%CD%\SDL2-2.0.3
set PKG_CONFIG_PATH=%SDL%\i686-w64-mingw32\lib\pkgconfig
set PATH=%SDL%\i686-w64-mingw32\bin;%PATH%
cabal install sdl2 --extra-lib-dirs=%SDL%\include --extra-include-dirs=%SDL%\lib\x86
