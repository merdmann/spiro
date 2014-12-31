#!/bin/bash

( ocamlbuild src/main.byte -- )
rm -rf ./build_

