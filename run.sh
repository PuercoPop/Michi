#!/bin/sh
sbcl --eval "(ql:quickload :michi)" \
     --eval "(michi:main)" \
     --eval "(progn (terpri) (sb-ext:quit))"
