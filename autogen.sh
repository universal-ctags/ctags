#!/bin/sh

misc/dist-test-cases test-cases.mak && \
  autoreconf -vfi

exit $?
