#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("monad/syntax" "monad/interface"
    "monad/Identity"
    "monad/List"
    "monad/State"
    "monad/Error"
    "monad/Parser"

    "Monad"))
