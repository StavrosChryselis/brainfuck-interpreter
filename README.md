# brainfuck-interpreter
An interpreter for the brainfuck programming language.

# requirements
Install the `haskell-stack` package from your favorite package manager.
f.e MacOs -> `brew install haskell-stack`

# running sample brainfuck code
`stack runhaskell brainfuck.hs < tests/input{X}.txt` where X is the test name index (1 - 7)

# running custom brainfuck code
`stack runhaskell brainfuck.hs < {input location}`

To run your `brainfuck` code dump it in a text file and put your input string on the first line,followed by the symbol `$`. Parsing ignores any character except from the official brainfuck commands ( > < . , + - [ ]) and whitespace, so you can easily write comments.
See the test cases for more info.
