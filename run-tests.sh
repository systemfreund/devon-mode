#!/bin/bash
emacs -Q --batch \
      -L . \
      -l ert \
      -l devon.el \
      -l tests/devon-tests.el \
      -l tests/checkpoint.el \
      -f ert-run-tests-batch-and-exit
