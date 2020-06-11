# mib-parser
This project implements a parser for minimal Basic, written in pure Rust, this leverages Pest, the elegant parser, see https://pest.rs

Running just Parse and AST, on the test suite which includes tests that are broken, with a release build gives:

    [2020-06-11T17:25:37Z ERROR mbasic] Parse failed for .\data\NBS\P003.BAS
    [2020-06-11T17:25:37Z ERROR mbasic] Parse failed for .\data\NBS\P004.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P020.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P036.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P037.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P038.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P050.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P051.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P052.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P053.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P054.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P079.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P102.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P103.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P104.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P105.BAS
    [2020-06-11T17:25:38Z ERROR mbasic] Parse failed for .\data\NBS\P106.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P113.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P143.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P144.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P145.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P147.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P149.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P150.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P155.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P156.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P157.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P158.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P159.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P185.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P187.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P188.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P189.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P190.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P191.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P192.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P193.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P194.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P195.BAS
    [2020-06-11T17:25:39Z ERROR mbasic] Parse failed for .\data\NBS\P199.BAS
    [2020-06-11T17:25:40Z ERROR mbasic] Parse failed for .\data\NBS\P201.BAS
    [2020-06-11T17:25:40Z ERROR mbasic] Parse failed for .\data\NBS\P204.BAS
    [2020-06-11T17:25:40Z ERROR mbasic] Parse failed for .\data\NBS\P206.BAS
    [2020-06-11T17:25:40Z ERROR mbasic] Parse failed for .\data\NBS\P207.BAS
    [2020-06-11T17:25:40Z ERROR mbasic] Parse failed for .\data\NBS\P208.BAS
    208 files read, 163 ok, 45 failed parse, in 2207ms
