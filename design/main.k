extern fn printf(fmt: string, val: i32): void
extern fn add(a: i32, b: i32): i32

fn main(): i32 {
    var result = add(10, 20);

    printf("Result is: %d\n", result);
    
    return 0;
}