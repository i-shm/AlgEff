```

BenchmarkDotNet v0.14.0, macOS 26.6.1 (25G76) [Darwin 25.6.0]
Apple M2, 1 CPU, 8 logical and 8 physical cores
  [Host]     : .NET 10.0.10 (10.0.1026.32716), Arm64 RyuJIT AdvSIMD DEBUG
  Job-ALOBRS : .NET 10.0.10 (10.0.1026.32716), Arm64 RyuJIT AdvSIMD

Toolchain=.NET 10.0  

```
| Method | Steps | Mean       | Error   | StdDev  | Gen0      | Gen1   | Allocated |
|------- |------ |-----------:|--------:|--------:|----------:|-------:|----------:|
| **Run**    | **1000**  |   **209.6 μs** | **1.01 μs** | **0.79 μs** |  **130.3711** | **1.2207** |   **1.04 MB** |
| **Run**    | **10000** | **2,105.5 μs** | **6.32 μs** | **5.28 μs** | **1300.7813** | **7.8125** |  **10.38 MB** |
