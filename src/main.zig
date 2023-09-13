const std = @import("std");
const print = std.debug.print;

pub fn read_line(reader: anytype) !?[]const u8 {
    var buffer: [100]u8 = undefined;
    return reader.readUntilDelimiterOrEof(&buffer, '\n');
}

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    for (0..3) |_| {
        const l = try read_line(stdin) orelse "";
        print("{s}\n", .{l});
    }
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // const stdin = std.io.getStdIn().reader();
    // var buf_reader = std.io.bufferedReader(stdin);
    // var istream = buf_reader.reader();
    // var buf: [1024]u8 = undefined;
    // while (try istream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
    //     try stdout.print("{s}", .{line});
    // }

    // const stdin = std.io.getStdIn().reader();
    // stdin.streamUntilDelimiter(writer: anytype, '\n')

    // // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // // stdout is for the actual output of your application, for example if you
    // // are implementing gzip, then only the compressed bytes should be sent to
    // // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});

    //    try bw.flush(); // don't forget to flush!
}

// test "simple test" {
//     var list = std.ArrayList(i32).init(std.testing.allocator);
//     defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
//     try list.append(42);
//     try std.testing.expectEqual(@as(i32, 42), list.pop());
// }
