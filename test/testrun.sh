#!/bin/bash
cd "`dirname "$0"`"

stack exec hanon-exe -- --help

stack exec hanon-exe -- --scan hello.txt

stack exec hanon-exe -- --list

