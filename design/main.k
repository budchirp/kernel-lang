extern fn printf(fmt: string, ...args: i32[]): void
extern fn malloc(size: i32): *void
extern fn free(ptr: *void): void

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

fn sum_array(...args: i32[]): i32 {
    return args[0] + args[1] + args[2];
}

fn main(): i32 {
    var ptr = allocate<Point>(Point {
        x: 10,
        y: 20
    });

    var sum = add<i32>((*ptr).x, (*ptr).y);    
    printf("Point sum: %d\n", sum);

    free(ptr);

    var x = 10;
    var y = 10;

    if (x == y) {
        printf("x is equal to y\n");
    } else {
        printf("x is not equal to y\n");
    }

    var arr_sum = sum_array(100, 200, 300);
    printf("Array sum: %d\n", arr_sum);

    return 0;
}
