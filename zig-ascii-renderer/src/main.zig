const std = @import("std");
const renderer = @import("renderer.zig");
const particle_system = @import("particle_system.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var screen = renderer.Screen.init();
    var particle_system_x = particle_system.ParticleSystem.init(allocator);
    defer particle_system_x.deinit();

    var frame: usize = 0;
    while (frame < 100) : (frame += 1) {
        // Clear the screen
        std.debug.print("\x1b[2J\x1b[H", .{});

        screen = renderer.Screen.init();

        try particle_system_x.emit(
            @as(f32, @floatFromInt(renderer.SCREEN_WIDTH)) / 2,
            @as(f32, @floatFromInt(renderer.SCREEN_HEIGHT)) / 2,
            '*',
            renderer.Color.rgb(255, @as(u8, (@intCast(frame * 6 % 256))), 0),
        );

        particle_system_x.update();
        particle_system_x.draw(&screen);

        screen.render();

        std.time.sleep(50 * std.time.ns_per_ms);
    }
}
