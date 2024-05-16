# sysy-hs
SysY2022 language implementation written in pure Haskell.

SysY2022语言的纯Haskell实现，由于是兴趣向项目，且目前忙于其他目标，所以进度会有点慢。

Haskell真的非常适合写这种项目，例如Parser只用了一晚上就写完了，还不容易出错，只是可惜大赛不能用Haskell。不过我猜如果Rust有parsec这种库，应该不难翻译到Rust。

# Targets
Work In Progress.

- [x] Parser
- [ ] Static Analysis
  - [x] Name Resolution
  - [x] Type Checking
  - [ ] Constant Computation (Or a part of data flow analysis?, but const array's dimensions should be resolved right after type checking)
  - [ ] Control Flow Analysis
  - [ ] Data Flow Analysis
- [ ] LLVM backend
- [ ] Optimizations
