//usr/bin/env zig run "$0" -- "$@"; exit

const std = @import("std");

const command = [_][]const u8{ "cargo", "run", "-p=classyc", "--" };

pub fn main() !void {
    // setup allocator
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // parse args
    const args = try std.process.argsAlloc(allocator);
    if (args.len < 2) {
        std.debug.print("usage: {s} <file> <forward-args>", .{std.fs.path.basename(args[0])});
        return;
    }
    // get file path
    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const cwd = try std.os.getcwd(&buf);
    const file_path = try std.fs.path.join(allocator, &[_][]u8{ cwd, args[1] });
    std.debug.print("got path: {s}", .{file_path});

    // fmt --file flag
    var buffer = try allocator.alloc(u8, "--file=".len + file_path.len);
    var writer = std.io.fixedBufferStream(buffer);
    try std.fmt.format(writer.writer(), "--file={s}", .{file_path});

    // create full rust command
    var full_command = try allocator.alloc([]const u8, command.len + args.len - 1);
    std.mem.copy([]const u8, full_command, &command);
    full_command[command.len] = buffer;
    std.mem.copy([]const u8, full_command[(command.len + 1)..], args[2..]);
    std.debug.print("running: {s}", .{full_command});

    // setup rust backtrace
    var env_map = try std.process.getEnvMap(allocator);
    try env_map.put("RUST_BACKTRACE", "1");

    // run compiler
    var child = std.ChildProcess.init(full_command, allocator);
    child.env_map = &env_map;

    _ = try child.spawnAndWait();
}
