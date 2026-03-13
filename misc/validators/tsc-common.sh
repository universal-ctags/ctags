#!/bin/sh
#
# tsc-common.sh - common code used in validator-tsc*
#
#  Copyright (c) 2026, Masatake YAMATO
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
# USA.
#
action=$1
input=$2
TARGET=$3
shift 3

case "$action" in
    is_runnable)
		type tsc > /dev/null 2>&1
		exit $?
	;;
    validate)
		tsc --noEmit "$input" --target $TARGET > /dev/null
		exit $?
	;;
	*)
		echo "$0: unknown action: $action" >&2
		exit 1
	;;
esac
