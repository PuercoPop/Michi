#!/bin/sh
sbcl --eval "(ql:quickload :michi-tests)" \
     --eval "(michi-tests:run-all-tests)" \
     --eval "(progn (terpri) (sb-ext:quit))"
