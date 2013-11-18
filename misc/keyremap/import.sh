#!/bin/sh

cli=/Applications/KeyRemap4MacBook.app/Contents/Applications/KeyRemap4MacBook_cli.app/Contents/MacOS/KeyRemap4MacBook_cli

$cli set repeat.wait 15
/bin/echo -n .
$cli set parameter.keyoverlaidmodifier_timeout 300
/bin/echo -n .
$cli set remap.drop_escape 1
/bin/echo -n .
$cli set private.command_to_escape 1
/bin/echo -n .
$cli set private.control_to_escape 1
/bin/echo -n .
/bin/echo
