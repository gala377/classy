//usr/bin/env zig run "$0" -- "$@"; exit

const std = @import("std");

const command = [_][]const u8{ "cargo", "run", "-p=classyc", "--" };

pub fn main() !void {
    // setup allocator
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // parse args
    const args = try parse_args(allocator);
    // get file path
    const file_path = try expand_file_path(allocator, args[1]);

    // fmt --file flag
    const expanded_file = try file_flag(allocator, file_path);

    // create full rust command
    const full_command = try create_full_command(allocator, args, expanded_file);

    try start_compiler(allocator, full_command);
}

const ParsingError = error{
    InvalidFormat,
};

fn parse_args(allocator: std.mem.Allocator) ![][:0]u8 {
    const cmd_args = try std.process.argsAlloc(allocator);
    if (cmd_args.len < 2) {
        std.debug.print("usage: {s} <file> <forward-args>", .{std.fs.path.basename(cmd_args[0])});
        return ParsingError.InvalidFormat;
    }
    return cmd_args;
}

fn expand_file_path(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const cwd = try std.os.getcwd(&buf);
    const file_path = try std.fs.path.join(allocator, &[_][]const u8{ cwd, path });
    std.debug.print("got path: {s}", .{file_path});
    return file_path;
}

fn file_flag(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var buffer = try allocator.alloc(u8, "--file=".len + path.len);
    var writer = std.io.fixedBufferStream(buffer);
    try std.fmt.format(writer.writer(), "--file={s}", .{path});
    return buffer;
}

fn create_full_command(allocator: std.mem.Allocator, args: []const []const u8, expanded_file: []const u8) ![]const []const u8 {
    var full_command = try allocator.alloc([]const u8, command.len + args.len - 1);
    std.mem.copy([]const u8, full_command, &command);
    full_command[command.len] = expanded_file;
    std.mem.copy([]const u8, full_command[(command.len + 1)..], args[2..]);
    std.debug.print("running: {s}", .{full_command});
    return full_command;
}

fn start_compiler(allocator: std.mem.Allocator, full_command: []const []const u8) !void {
    var env_map = try std.process.getEnvMap(allocator);
    try env_map.put("RUST_BACKTRACE", "1");

    // run compiler
    var child = std.ChildProcess.init(full_command, allocator);
    child.env_map = &env_map;

    _ = try child.spawnAndWait();
}
