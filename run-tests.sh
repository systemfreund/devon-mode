#!/bin/bash
emacs -Q --batch \
      -L . \
      -l ert \
      -l devon.el \
      -l tests/test-devon-status.el \
      -f ert-run-tests-batch-and-exit
