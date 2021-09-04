TAPE_SIZE 30000;
CELL_SIZE 256;

free(v) {
    if (v == 0) {
        return;
    }
    rlsevec(v, -1);
}

realloc(v, n) {
    if (n == 0) {
        return;
    }
    auto new; new = getvec(n - 1);
    auto i; i = 0;
    while (i < n) {
        new[i] = v[i];
        ++i;
    }
    free(v);
    return ( new );
}

append(v, n, x) {
    auto new; new = realloc(v, n + 1);
    new[n] = x;
    return ( new );
}

advance_back(arr, i) {
    auto n; n = 0;
    while (i != 0) {
        switch (arr[i]) {
            case ']':
                --n;
                break;
            case '[':
                if (n == 0) {
                    return ( i );
                }
                ++n;
                break;
            default:
                break;
        }
        --i;
    }
    return ( i );
}

is_valid_command(c) {
    switch (c) {
        case '+':
        case '-':
        case '>':
        case '<':
        case '.':
        case ',':
        case '[':
        case ']':
            return ( 1 );
        default:
            return ( 0 );
    }
}

print_tape(tape, n) {
    if (n <= 0) {
        return;
    }
    print_num(tape[0]);
    auto i; i = 1;
    while (i < n) {
        putchar(' ');
        print_num(tape[i]);
        ++i;
    }
}

print_num(x) {
    printf("%d", x);
}

main() {
    extrn TAPE_SIZE, CELL_SIZE;
    auto tape; tape = getvec(TAPE_SIZE - 1);
    auto i; i = 0;
    auto skip; skip = 0;
    auto nesting; nesting = 0;
    auto target_nesting; target_nesting = 0;
    auto commands; commands = 0;
    auto commands_len; commands_len = 0;
    auto command_idx; command_idx = 0;
    while (1) {
        auto c;
        if (command_idx < commands_len) {
            c = commands[command_idx];
        } else {
            c = getchar();
            if (!is_valid_command(c)) {
                continue;
            }
            if (commands != 0) {
                commands = append(commands, commands_len, c);
                ++commands_len;
            }
        }
        if (skip) {
            switch (c) {
                case '[':
                    ++nesting;
                    break;

                case ']':
                    --nesting;
                    if (nesting == target_nesting) {
                        skip = 0;
                        if (target_nesting != 0) {
                            ++command_idx;
                        } else if (commands != 0) {
                            rlsevec(commands, commands_len - 1);
                            commands = 0;
                            commands_len = 0;
                            command_idx = 0;
                        }
                        continue;
                    }
                    break;

                default:
                    break;
            }
        } else {
            switch (c) {
                case '+':
                    if (tape[i] == CELL_SIZE - 1) {
                        tape[i] = 0;
                    } else {
                        ++tape[i];
                    }
                    break;

                case '-':
                    if (tape[i] == 0) {
                        tape[i] = CELL_SIZE - 1;
                    } else {
                        --tape[i];
                    }
                    break;

                case '>':
                    if (i == TAPE_SIZE - 1) {
                        i = 0;
                    } else {
                        ++i;
                    }
                    break;

                case '<':
                    if (i == 0) {
                        i = TAPE_SIZE - 1;
                    } else {
                        --i;
                    }
                    break;

                case '.':
                    putchar(tape[i]);
                    break;
                case ',':
                    tape[i] = getchar();
                    break;

                case '[':
                    ++nesting;
                    if (tape[i] == 0) {
                        skip = 1;
                        target_nesting = nesting - 1;
                    } else if (commands_len == 0) {
                        commands = getvec(0);
                        commands[0] = c;
                        commands_len = 1;
                        command_idx = 1;
                        continue;
                    }
                    break;

                case ']':
                    command_idx = advance_back(commands, command_idx - 1);
                    --nesting;
                    continue;

                default:
                    break;
            }
        }
        if (commands != 0) {
            ++command_idx;
        }
    }
}