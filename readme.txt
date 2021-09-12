The B programming language compiler written in Rust

Usage: b-compiler INPUT_FILE [--out=OUTPUT_FILE]

Options:
--ir — generate intermediate representation instead of machine code
(W) --stack=STACK_SIZE — set the stack size equal to STACK_SIZE. The default value is 4096
(W) --heap=HEAP_SIZE — set the heap size equal to HEAP_SIZE. The default value is 65536
--enable-continue — enable the continue statement. The original B did not support it
--platform=PLATFORM — set the target platform. The only available options are "win", "linux". The case is ignored
--arch=ARCH — set the target architecture. The only available options are "x86-64", "x64", "x86_64", "amd64". The case is ignored

Options denoted with (W) are Windows-only.