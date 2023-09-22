const std = @import("std");
const common = @import("common.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    while (rl(stdin)) |line| {
        std.debug.print("{s}\n", .{line});
        std.time.sleep(1000_0000);
    }
}

var buffer: [100]u8 = undefined;

fn readLine(reader: anytype) !?[]const u8 {
    var fbs = std.io.fixedBufferStream(&buffer);
    try reader.streamUntilDelimiter(fbs.writer(), '\n', null);
    return fbs.getWritten();
}

fn rl(reader: anytype) ?[]const u8 {
    return readLine(reader) catch null;
}
