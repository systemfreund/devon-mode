#!/bin/bash
emacs -Q --batch \
      -L . \
      -l ert \
      -l devon.el \
      -l tests/test-devon-stream.el \
      -l tests/devon-tests.el \
      -f ert-run-tests-batch-and-exit
