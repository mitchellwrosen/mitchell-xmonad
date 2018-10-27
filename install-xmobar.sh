#!/bin/sh

stack install xmobar \
  --flag xmobar:with_alsa \
  --flag xmobar:with_dbus \
  --flag xmobar:with_iwlib \
  --flag xmobar:-with_weather \
  --flag xmobar:with_xft
