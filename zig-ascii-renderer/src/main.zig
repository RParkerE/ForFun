const std = @import("std");

const Color = struct {
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

const Pixel = struct {
    char: u8,
    color: Color,
};

const SCREEN_WIDTH: usize = 80;
const SCREEN_HEIGHT: usize = 24;

const Screen = struct {
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

const Particle = struct {
    x: f32,
    y: f32,
    vx: f32,
    vy: f32,
    char: u8,
    color: Color,
    lifetime: u32,

    pub fn update(self: *Particle) void {
        self.x += self.vx;
        self.y += self.vy;
        self.lifetime -|= 1; // Saturating subtraction
    }
};

const ParticleSystem = struct {
    particles: std.ArrayList(Particle),

    pub fn init(allocator: std.mem.Allocator) ParticleSystem {
        return ParticleSystem{
            .particles = std.ArrayList(Particle).init(allocator),
        };
    }

    pub fn deinit(self: *ParticleSystem) void {
        self.particles.deinit();
    }

    pub fn emit(self: *ParticleSystem, x: f32, y: f32, char: u8, color: Color) !void {
        const particle = Particle{
            .x = x,
            .y = y,
            .vx = (std.crypto.random.float(f32) - 0.5) * 2,
            .vy = (std.crypto.random.float(f32) - 0.5) * 2,
            .char = char,
            .color = color,
            .lifetime = 20 + std.crypto.random.intRangeAtMost(u32, 0, 40),
        };
        try self.particles.append(particle);
    }

    pub fn update(self: *ParticleSystem) void {
        var i: usize = 0;
        while (i < self.particles.items.len) {
            self.particles.items[i].update();
            if (self.particles.items[i].lifetime == 0) {
                _ = self.particles.swapRemove(i);
            } else {
                i += 1;
            }
        }
    }

    pub fn draw(self: ParticleSystem, screen: *Screen) void {
        for (self.particles.items) |particle| {
            const x = @as(usize, @intFromFloat(particle.x));
            const y = @as(usize, @intFromFloat(particle.y));
            screen.drawPixel(x, y, Pixel{ .char = particle.char, .color = particle.color });
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var screen = Screen.init();
    var particle_system = ParticleSystem.init(allocator);
    defer particle_system.deinit();

    var frame: usize = 0;
    while (frame < 100) : (frame += 1) {
        screen = Screen.init();

        try particle_system.emit(
            @as(f32, @floatFromInt(SCREEN_WIDTH)) / 2,
            @as(f32, @floatFromInt(SCREEN_HEIGHT)) / 2,
            '*',
            Color.rgb(255, @as(u8, @intCast(frame * 2)), 0),
        );

        particle_system.update();
        particle_system.draw(&screen);

        screen.render();

        std.time.sleep(50 * std.time.ns_per_ms);

        std.debug.print("\x1b[2J\x1b[H", .{});
    }
}
