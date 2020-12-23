/* Taken from https://github.com/ueno/libskk
 * (libskk/libskk/key-event.vala)
 */

/*
 * Copyright (C) 2011-2018 Daiki Ueno <ueno@gnu.org>
 * Copyright (C) 2011-2018 Red Hat, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
using Gee;

namespace Skk {
    public errordomain KeyEventFormatError {
        PARSE_FAILED,
        KEYSYM_NOT_FOUND
    }

    /**
     * A set of bit-flags to indicate the state of modifier keys.
     */
    [Flags]
    public enum ModifierType {
        NONE = 0,
        SHIFT_MASK = 1 << 0,
        LOCK_MASK = 1 << 1,
        CONTROL_MASK = 1 << 2,
        MOD1_MASK = 1 << 3,
        MOD2_MASK = 1 << 4,
        MOD3_MASK = 1 << 5,
        MOD4_MASK = 1 << 6,
        MOD5_MASK = 1 << 7,

        // dummy modifiers for NICOLA
        LSHIFT_MASK = 1 << 22,
        RSHIFT_MASK = 1 << 23,
        USLEEP_MASK = 1 << 24,

        SUPER_MASK = 1 << 26,
        HYPER_MASK = 1 << 27,
        META_MASK = 1 << 28,
        RELEASE_MASK = 1 << 30
    }

	public enum ModifierType2 {
        ULTRA_MASK = 1 << 20,
        MIRACLE_MASK = 1 << 21,	// valac accept this.
	}

	public enum ModifierType3 {
        A_MASK = 1 << 29,
        B_MASK = 1 << 31;
		string to_string0() {
			return "";
		}
		string to_string1() {
			return "";
		}
	}

    /**
     * Object representing a key event.
     */
    public class KeyEvent : Object {
		/* ... */
    }
}
