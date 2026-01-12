extern fn printf(fmt: string, ...args: i32): void
extern fn malloc(size: i32): *void

struct Point {
    x: i32
    y: i32
}

fn add<T: i32>(a: T, b: T): T {
    return a + b;
}

fn allocate<T>(initializer: T): *T {
    var ptr = malloc(128) as *T;
    *ptr = initializer;
    return ptr;
}

fn main(): i32 {
    var ptr = allocate<Point>(Point {
        x: 10,
        y: 20
    });

    var sum = add<i32>((*ptr).x, (*ptr).y);
    printf("Point sum: %d\n", sum);
    printf("Multiple args: %d + %d = %d\n", 5, 7, add<i32>(5, 7));

    return 0;
}
