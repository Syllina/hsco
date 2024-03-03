optparse-applicative

## Parsers

一个例子
```haskell
target :: Parser String
target = strOption
  (  long "hello"
  <> metavar "TARGET"
  <> help "Target for the greeting" )
```

该 parser 对应命令行中的
```
--hello world
```

生成 parser 使用的 `strOption` 函数接受的参数被称为 *modifiers*，可以使用 `(<>)` 组合.

如果构建了 parse 某个数据类型各个部分的 parser，可以使用 `Applicative` 的方法将它们组合为完整的数据类型的 parser.

由于 `Applicative` 的性质，这里的各个部分的顺序是无关紧要的.（为什么？每个部分的值无法依赖于其他部分）

（使用 `Arrow` interface 似乎可以避免 `Applicative` 对 field 顺序的要求？）

对于 *sum types*，我们可以相应地使用 `Alternative` 的方法（parse 的时候会阻止两个部分同时出现）.

### Running parsers
运行 `Parser` 之前需要将它 wrap 在 `ParserInfo` 之中.
```haskell
helper :: Parser (a -> a)
```
`helper` 是一个永远会失败，但是包含了 `--help` 的 parser.（可以认为它包含的函数是 `id`？为什么没有一个描述了这种行为的函数？）

最常见的运行 `ParserInfo` 的方法是调用 `execParser`. 当然还有其他调用的方法.

## Builders
builders 是用常见的函数来构造 parser 的方法（或者说 builder 就是这个函数）. 它会接受 modifiers 用来描述 parser 的性质.

### Regular options

这是一个接受单独一个参数的选项. 常见的例子如

```haskell
strOption
   ( long "output"
  <> short 'o'
  <> metavar "FILE"
  <> value "out.txt"
  <> help "Write output to FILE" )
```

其中 `metavar` 是在帮助和文档等地方 refer 的名字. `value` 是选项的默认值.

一般的 `option` 接受一个 reader，常见的 reader 包括 `auto`，它能解析任何有 `Read` 实例的类型.

### Flags

这是一个不接受参数的选项. 它（`flag`）需要一个默认值和一个（用户指定了该参数之后采用的）值. 一般的 `Bool` 类型 flag 可以直接使用 `switch` 构建.

存在一个 `flag'` builder，它不接受默认值.

### Arguments

`argument` 接受一个有位置性的命令行参数.

### Commands

`subparser` 接受一系列用 `command` 包装的，返回 *相同值* 的 `ParserInfo`，组成一个新的 `Parser`. 为了保证返回相同值，这里可以采用构建一个 `sum type` 的方式，也可以直接返回 IO action.

