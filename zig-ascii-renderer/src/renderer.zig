const std = @import("std");

pub const Color = struct {
    r: u8,
    g: u8,
    b: u8,

    pub fn rgb(r: u8, g: u8, b: u8) Color {
        return Color{ .r = r, .g = g, .b = b };
    }

    pub fn ansiCode(self: Color) []const u8 {
        return std.fmt.allocPrint(std.heap.page_allocator, "\x1b[38;2;{d};{d};{d}m", .{ self.r, self.g, self.b }) catch unreachable;
    }
};

pub const Pixel = struct {
    char: u8,
    color: Color,
};

pub const SCREEN_WIDTH: usize = 80;
pub const SCREEN_HEIGHT: usize = 24;

pub const Screen = struct {
    buffer: [SCREEN_HEIGHT][SCREEN_WIDTH]Pixel,

    pub fn init() Screen {
        var screen = Screen{
            .buffer = undefined,
        };
        for (&screen.buffer) |*row| {
            for (row) |*pixel| {
                pixel.* = Pixel{ .char = ' ', .color = Color.rgb(255, 255, 255) };
            }
        }
        return screen;
    }

    pub fn drawPixel(self: *Screen, x: usize, y: usize, pixel: Pixel) void {
        if (x < SCREEN_WIDTH and y < SCREEN_HEIGHT) {
            self.buffer[y][x] = pixel;
        }
    }

    pub fn render(self: Screen) void {
        for (self.buffer) |row| {
            for (row) |pixel| {
                const color_code = pixel.color.ansiCode();
                defer std.heap.page_allocator.free(color_code);
                std.debug.print("{s}{c}\x1b[0m", .{ color_code, pixel.char });
            }
            std.debug.print("\n", .{});
        }
    }
};
