import argparse
import re
import sys


def find_subroutine(file, subroutine_name):
    section_started = False
    section_ended = False
    subroutine_started = False

    subroutine_content = []

    for line in file:
        #print(f"{section_started=}, {section_ended=}, {subroutine_started=}, {line=}")

        if line.startswith('---'):
            if section_started:
                section_ended = True
                section_started = False
                continue

            if not section_started:
                section_started = True
                section_ended = False
                continue

        if section_started and line.startswith(f"{subroutine_name}:"):
            subroutine_started = True

        if section_ended and subroutine_started:
            return subroutine_content

        if subroutine_started:
            subroutine_content.append(line)

    raise ValueError("Not ending with section end marker, wrong format?")


opcode_line_pattern = re.compile(r'([0-9a-f]+) : ((?:[0-9a-f_]{2} ){3})(.*)')

def main(args):
    subroutine_lines = find_subroutine(args.file, args.name)
    processed_lines = []
    address_to_label = {}
    current_label = None

    # first pass: scan for addresses and their corresponding labels.
    # we assume that the label (.foo) is followed by a line with an address
    for line in subroutine_lines:
        if line.startswith('.'):
            current_label = line.strip()
            continue
        match = opcode_line_pattern.match(line)
        if match and current_label is not None:
            address, *_ = match.groups()
            address_to_label[address] = current_label
            current_label = None

    # second pass: filter nonsense and translate addresses with labels
    for line in subroutine_lines:
        #print(line, end='')

        if line.startswith(';'):
            continue

        match = opcode_line_pattern.match(line)
        if match:
            # ('08d8', '86 1b __ ', 'STX ACCU + 0 ')
            address, _opcodes, assembly = match.groups()

            parts = assembly.split(" ")
            if len(parts) > 1 and parts[1].startswith('$'):
                # default to the original address for cases where we load/store
                # from global addresses, e.g. LDA $d415
                parts[1] = address_to_label.get(parts[1][1:], parts[1])
                assembly = " ".join(parts)

            processed_lines.append(assembly + "\n")
            continue

        processed_lines.append(line)

    # we expect the including assembly to wrap the code in its own subroutine
    # so we can strip the first line which includes the name of the subroutine.
    for line in processed_lines[1:]:
        print(line, end='')



if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=argparse.FileType('r'))
    parser.add_argument("name", type=str, help="Name of subroutine")

    args = parser.parse_args()

    main(args)
