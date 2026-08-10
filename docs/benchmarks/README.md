# BenchmarkDotNet Reports

These reports capture the AlgEff 2.0 runtime benchmarks on the primary development target.

## 2026-08-10, .NET 10

Environment:

- BenchmarkDotNet 0.14.0
- macOS 26.6.1
- Apple M2, Arm64 RyuJIT
- .NET 10.0.10 runtime

Command:

```bash
dotnet run --project AlgEff.Bench/AlgEff.Bench.fsproj -c Release -f net10.0 -- --filter '*'
```

Reports:

- [State handler](state-net10-2026-08-10.md)
- [Combined dispatch](combined-dispatch-net10-2026-08-10.md)
- [NonDet](nondet-net10-2026-08-10.md)
