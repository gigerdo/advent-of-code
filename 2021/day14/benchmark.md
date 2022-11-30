Benchmark

```
# Benchmarking needs a special test (main_test.go)
go test -cpuprofile cpu.prof -memprofile mem.prof -bench .
```

pprof
```
# Start pprof
go tool pprof cpu.prof

# Some useful commands
list <funcname>
top10
...
```