#!/usr/bin/env python3
"""Require an ELF64 executable to declare GNU's x86-64-v3 ISA floor."""

from __future__ import annotations

import argparse
import struct
from pathlib import Path


GNU_PROPERTY_X86_ISA_1_NEEDED = 0xC0008002
X86_64_V3_MASK = 0x7  # baseline | v2 | v3
NT_GNU_PROPERTY_TYPE_0 = 5


def align(value: int, alignment: int) -> int:
    return (value + alignment - 1) & -alignment


def section(data: bytes, name: str) -> tuple[bytes, int]:
    if data[:4] != b"\x7fELF" or data[4] != 2 or data[5] != 1:
        raise ValueError("expected a little-endian ELF64 executable")
    shoff = struct.unpack_from("<Q", data, 40)[0]
    shentsize, shnum, shstrndx = struct.unpack_from("<HHH", data, 58)
    if not shoff or not shnum or shstrndx >= shnum:
        raise ValueError("ELF section table is absent or malformed")

    def header(index: int) -> tuple[int, int, int, int]:
        offset = shoff + index * shentsize
        name_offset = struct.unpack_from("<I", data, offset)[0]
        file_offset, size = struct.unpack_from("<QQ", data, offset + 24)
        alignment = struct.unpack_from("<Q", data, offset + 48)[0]
        return name_offset, file_offset, size, alignment

    _, strings_offset, strings_size, _ = header(shstrndx)
    strings = data[strings_offset : strings_offset + strings_size]
    for index in range(shnum):
        name_offset, file_offset, size, alignment = header(index)
        end = strings.find(b"\0", name_offset)
        if end >= 0 and strings[name_offset:end].decode("ascii", "strict") == name:
            return data[file_offset : file_offset + size], max(alignment, 4)
    raise ValueError(f"missing {name} section")


def isa_needed_mask(binary: Path) -> int:
    notes, note_alignment = section(binary.read_bytes(), ".note.gnu.property")
    position = 0
    combined_mask = 0
    while position + 12 <= len(notes):
        namesz, descsz, note_type = struct.unpack_from("<III", notes, position)
        name_start = position + 12
        name_end = name_start + namesz
        desc_start = align(name_end, note_alignment)
        desc_end = desc_start + descsz
        if desc_end > len(notes):
            raise ValueError("truncated GNU property note")
        if note_type == NT_GNU_PROPERTY_TYPE_0 and notes[name_start:name_end] == b"GNU\0":
            prop = desc_start
            while prop + 8 <= desc_end:
                prop_type, prop_size = struct.unpack_from("<II", notes, prop)
                value_start = prop + 8
                value_end = value_start + prop_size
                if value_end > desc_end:
                    raise ValueError("truncated GNU property descriptor")
                if prop_type == GNU_PROPERTY_X86_ISA_1_NEEDED:
                    if prop_size != 4:
                        raise ValueError("GNU x86 ISA-needed property is not four bytes")
                    combined_mask |= struct.unpack_from("<I", notes, value_start)[0]
                prop = align(value_end, 8)
        position = align(desc_end, note_alignment)
    return combined_mask


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", type=Path)
    args = parser.parse_args()
    mask = isa_needed_mask(args.binary)
    if mask & X86_64_V3_MASK != X86_64_V3_MASK:
        raise SystemExit(
            f"{args.binary}: GNU x86 ISA-needed mask is 0x{mask:x}, expected v3 mask 0x7"
        )
    print(f"{args.binary}: GNU x86 ISA-needed mask 0x{mask:x} includes x86-64-v3")


if __name__ == "__main__":
    main()
