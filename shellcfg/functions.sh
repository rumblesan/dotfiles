#! /bin/bash

# Some useful functions

repos() {
  cd "${REPODIR}/$1"
}

function groot() {

  cd $(git rev-parse --show-toplevel)

}
