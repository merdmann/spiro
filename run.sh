#!/bin/bash
( cd src && make clean && make )
ocamlbuild -build-dir ./build -lib graphics src/main.byte --


