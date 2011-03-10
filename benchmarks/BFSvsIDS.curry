-- Benchmark to compare BFS vs. IDS

f n = f (n+1) ? n

main | f 0 == 25000 = success

