#!/bin/bash
STATE="$(fcitx-remote)"

case $STATE in
1*)
  echo "US";;
2*)
  echo "CN";;
*)
  echo "ER";;
esac
