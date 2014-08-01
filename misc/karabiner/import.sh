#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set remap.f11_to_volumedown_all 1
/bin/echo -n .
$cli set remap.f10_to_musicnext_all 1
/bin/echo -n .
$cli set remap.drop_escape 1
/bin/echo -n .
$cli set remap.f9_to_musicplay_all 1
/bin/echo -n .
$cli set private.command_to_escape 1
/bin/echo -n .
$cli set remap.f8_to_musicprev_all 1
/bin/echo -n .
$cli set repeat.wait 15
/bin/echo -n .
$cli set remap.f12_to_volumeup_all 1
/bin/echo -n .
$cli set parameter.keyoverlaidmodifier_timeout 300
/bin/echo -n .
$cli set private.control_to_escape 1
/bin/echo -n .
/bin/echo
