# Lexer 模块设计文档

## 概要

Lexer 是 RustLuaJIT 中处理 Lua 源码中关键的一个模块。
Lexer 模块主要负责词法分析，用户将 Lua 源码输入到系统中，该模块会将 Lua 源码拆解为若干 Token，供 Parser 使用。[Lexical analysis](https://en.wikipedia.org/wiki/Lexical_analysis)

### 目标

* 提供类似“游标”的机制，能够通过 `next` 方法获取下一个 Token；
* 能够对每个 Token 归纳类型，供 Parser 模块执行语法分析。

