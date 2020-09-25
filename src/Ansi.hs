module Ansi where

newtype Ansi = Ansi {unAnsi :: String}

_RESET_ :: Ansi
_RESET_ = Ansi "\o33[0;0m"

_BOLD_ :: Ansi
_BOLD_ = Ansi "\o33[1m"

_REVERSE_ :: Ansi
_REVERSE_ = Ansi "\o33[2m"

_BLACK_ :: Ansi
_BLACK_ = Ansi "\o33[0;30m"

_BLUE_ :: Ansi
_BLUE_ = Ansi "\o33[0;34m"

_GREEN_ :: Ansi
_GREEN_ = Ansi "\o33[0;32m"

_CYAN_ :: Ansi
_CYAN_ = Ansi "\o33[0;36m"

_RED_ :: Ansi
_RED_ = Ansi "\o33[0;31m"

_PURPLE_ :: Ansi
_PURPLE_ = Ansi "\o33[0;35m"

_BROWN_ :: Ansi
_BROWN_ = Ansi "\o33[0;33m"

_GRAY_ :: Ansi
_GRAY_ = Ansi "\o33[0;37m"

_DARK_GRAY_ :: Ansi
_DARK_GRAY_ = Ansi "\o33[1;30m"

_LIGHT_BLUE_ :: Ansi
_LIGHT_BLUE_ = Ansi "\o33[1;34m"

_LIGHT_GREEN_ :: Ansi
_LIGHT_GREEN_ = Ansi "\o33[1;32m"

_LIGHT_CYAN_ :: Ansi
_LIGHT_CYAN_ = Ansi "\o33[1;36m"

_LIGHT_RED_ :: Ansi
_LIGHT_RED_ = Ansi "\o33[1;31m"

_LIGHT_PURPLE_ :: Ansi
_LIGHT_PURPLE_ = Ansi "\o33[1;35m"

_WHITE_ :: Ansi
_WHITE_ = Ansi "\o33[1;37m"

colorize :: Ansi -> String -> String
colorize (Ansi prefix) t = prefix <> t <> unAnsi _RESET_
