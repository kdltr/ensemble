#!/bin/sh

set -e

extract_dependencies() {
    csi -e "(for-each print (alist-ref 'dependencies (with-input-from-file \"ensemble.egg\" read)))" | grep -Ev '(ncurses|bind)'
}

build_normal_deps() {
    extract_dependencies | xargs chicken-install
}

build_custom_dep() {
    echo "Building $1…"
    cd "$1";
    chicken-install
}

cd "`dirname $0`"

CHICKEN_REPOSITORY_PATH="`pwd`:`pwd`/repo:`csi -R chicken.platform -p '(car (repository-path))'`"
export CHICKEN_REPOSITORY_PATH

export CHICKEN_INSTALL_REPOSITORY=`pwd`/repo
mkdir -p $CHICKEN_INSTALL_REPOSITORY

test -d .git && git submodule update --init

( build_custom_dep bind-egg )
( build_custom_dep chicken-ncurses )
build_normal_deps

# Build Ensemble itself (without installing)
chicken-install -n -D release

echo "Ensemble has been built, yay! \o/"
