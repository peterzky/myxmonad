#!/bin/bash
pico2wave -w /tmp/test.wav "$(xsel)"
aplay /tmp/test.wav
rm /tmp/test.wav
