package add;
import org.openjdk.jmh.annotations.Benchmark;

import java.util.concurrent.TimeUnit;


import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 10, time =1 )
@Fork(1)
@Measurement(iterations = 10, time = 1)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Mode.AverageTime)
public class SoFlow {
    @Param({"10000" })
    private int size;

    @Benchmark
    public int test_two_square_i() {
        int n = 0;
        for (int i = 0; i < size; i++) {
            n += 2 * (i * i);
        }
        return n;
    }

    @Benchmark
    public int test_square_i_two() {
        int n = 0;
        for (int i = 0; i < size; i++) {
            n += i * i;
        }
        return 2*n;
    }

    @Benchmark
    public int test_two_i_() {
        int n = 0;
        for (int i = 0; i < size; i++) {
            n += 2 * i * i;
        }
        return n;
    }
}
